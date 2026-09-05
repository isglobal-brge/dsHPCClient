# Module: Job Outputs

#' List outputs registered for a dsHPC job
#'
#' @param conns DSI connections object.
#' @param job_id Character; a public `trk_...` tracking id, a private workflow
#'   symbol/bearer, or a named per-server vector. The shared path lists only
#'   outputs classified `client_safe` or `server_reusable`.
#' @return A `dshpc_result` with one output metadata data.frame per site
#'   (shared columns are `name`, `kind`, and `classification`).
#' @examples
#' \dontrun{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' ds.hpc.outputs(conns, "jobA")
#' }
#' @export
ds.hpc.outputs <- function(conns, job_id) {
  results <- .ds_safe_aggregate(conns,
    expr = function(srv) {
      reference <- .ds_job_reference_for_site(job_id, srv)
      method <- if (.ds_is_tracking_id(as.character(reference))) {
        "hpcTrackingOutputsDS"
      } else "hpcOutputsDS"
      call(method, reference)
    })
  tracking_sites <- names(results)[vapply(names(results), function(server) {
    reference <- tryCatch(.ds_job_reference_for_site(job_id, server),
      error = function(e) NULL)
    !is.null(reference) && .ds_is_tracking_id(as.character(reference))
  }, logical(1))]
  results <- .ds_transform_site_results(results, .ds_tracking_outputs,
    sites = tracking_sites)
  dshpc_result(per_site = results)
}

#' Assign a shared output reference in the server session
#'
#' Creates an opaque `dshpc_output_reference` in each selected server session.
#' The output value, artifact path and underlying execution job never cross to
#' the client. Trusted server packages can consume the reference in later
#' pipelines, and every client-facing result remains subject to disclosure
#' control.
#'
#' @param conns DSI connections object.
#' @param tracking_id Public scalar `trk_...` id, or a vector naming one id for
#'   every connection.
#' @param output_name Fixed public alias returned by [ds.hpc.outputs()]:
#'   `output_001` or `output_002`.
#' @param symbol Optional visible R symbol to create in the server sessions.
#'   By default a valid symbol is derived from `output_name`.
#' @return `TRUE`, invisibly. No output value is downloaded.
#' @examples
#' \dontrun{
#' ds.hpc.load_output(conns["site1"], "trk_...", "output_001",
#'   symbol = "shared_features")
#' }
#' @export
ds.hpc.load_output <- function(conns, tracking_id, output_name,
                               symbol = NULL) {
  output_name <- .ds_shared_output_name(output_name)
  if (is.null(symbol)) symbol <- .ds_default_output_symbol(output_name)
  symbol <- .ds_output_symbol(symbol)
  servers <- names(conns)
  if (is.null(servers) || anyNA(servers) || any(!nzchar(servers))) {
    stop("DataSHIELD connections require non-empty node names.", call. = FALSE)
  }
  expressions <- stats::setNames(lapply(servers, function(server) {
    id <- .ds_tracking_id_for_site(tracking_id, server)
    call("hpcTrackingAssignOutputDS", id, output_name)
  }), servers)
  .ds_private_assign(conns, symbol = symbol, expr = expressions,
    operation = "Shared output assignment")
  invisible(TRUE)
}

#' @keywords internal
.ds_shared_output_name <- function(output_name) {
  output_name <- .ds_output_name(output_name)
  if (!output_name %in% .DSHPC_SHARED_OUTPUT_NAMES) {
    stop("A valid shared output alias is required.", call. = FALSE)
  }
  output_name
}

#' @keywords internal
.ds_default_output_symbol <- function(output_name) {
  output_name <- .ds_output_name(output_name)
  symbol <- gsub("[^A-Za-z0-9._]", ".", output_name)
  if (!grepl("^[A-Za-z]", symbol) ||
      !identical(make.names(symbol), symbol)) {
    symbol <- paste0("output.", symbol)
  }
  .ds_output_symbol(substr(symbol, 1L, 96L))
}

#' @keywords internal
.ds_output_name <- function(output_name) {
  if (!is.character(output_name) || length(output_name) != 1L ||
      is.na(output_name) || !nzchar(output_name) ||
      nchar(output_name, type = "bytes") > 128L ||
      grepl("\\.\\.", output_name) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", output_name)) {
    stop("A valid shared output name is required.", call. = FALSE)
  }
  output_name
}

#' @keywords internal
.ds_output_symbol <- function(symbol) {
  if (!is.character(symbol) || length(symbol) != 1L || is.na(symbol) ||
      !grepl("^[A-Za-z][A-Za-z0-9._]{0,95}$", symbol) ||
      !identical(make.names(symbol), symbol)) {
    stop("A visible DataSHIELD symbol beginning with a letter is required.",
      call. = FALSE)
  }
  symbol
}

#' Report dsHPC server capabilities
#'
#' @param conns DSI connections object.
#' @return A `dshpc_result` with per-site capability lists.
#' @export
ds.hpc.capabilities <- function(conns) {
  results <- .ds_safe_aggregate(conns, expr = call("hpcCapabilitiesDS"))
  dshpc_result(per_site = results)
}

#' Retired analyst scheduler status
#'
#' Scheduler topology is node-operator information and is no longer queried
#' through an analyst DataSHIELD connection.
#'
#' @param conns Ignored compatibility argument.
#' @return This function always raises an error before contacting a server.
#' @export
ds.hpc.scheduler_status <- function(conns) {
  stop(
    "Analyst scheduler topology is retired; use node-local administration.",
    call. = FALSE
  )
}

#' Authorize a dsHPC job without disclosing runner logs
#'
#' @param conns DSI connections object.
#' @param job_id Character; scalar workflow symbol/bearer, or the named
#'   per-server bearer vector returned by `ds.hpc.job_id()`.
#' @param last_n Integer number of log lines requested from each site.
#' @return A `dshpc_result` with an empty character vector per authorized site.
#'   Raw runner output remains node/operator-only.
#' @examples
#' \dontrun{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' ds.hpc.logs(conns, "jobA", last_n = 20)
#' }
#' @export
ds.hpc.logs <- function(conns, job_id, last_n = 50L) {
  if (is.character(job_id) && any(vapply(as.list(job_id),
      .ds_is_tracking_id, logical(1)))) {
    stop("Runner logs are not available through shared tracking ids.",
      call. = FALSE)
  }
  results <- .ds_safe_aggregate(conns,
    expr = function(srv) call("hpcLogsDS",
      .ds_job_reference_for_site(job_id, srv), as.integer(last_n)))
  dshpc_result(per_site = results)
}

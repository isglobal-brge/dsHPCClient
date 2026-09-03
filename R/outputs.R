# Module: Job Outputs

#' List outputs registered for a dsHPC job
#'
#' @param conns DSI connections object.
#' @param job_id Character; opaque bearer or domain workflow symbol (see
#'   `ds.hpc.job_id()`).
#' @return A `dshpc_result` with one output metadata data.frame per site
#'   (columns `name`, `kind`, `safe_for_client`, `size_bytes`); printed as
#'   name/kind/size rows.
#' @examples
#' \dontrun{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' ds.hpc.outputs(conns, "jobA")
#' }
#' @export
ds.hpc.outputs <- function(conns, job_id) {
  results <- .ds_safe_aggregate(conns,
    expr = call("hpcOutputsDS", job_id))
  dshpc_result(per_site = results)
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
#' @param job_id Character; opaque bearer or domain workflow symbol (see
#'   `ds.hpc.job_id()`).
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
  results <- .ds_safe_aggregate(conns,
    expr = call("hpcLogsDS", job_id, as.integer(last_n)))
  dshpc_result(per_site = results)
}

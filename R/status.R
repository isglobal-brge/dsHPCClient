# Module: Job Status

#' Get dsHPC job status
#'
#' @param conns DSI connections object.
#' @param job_id Character; a public `trk_...` tracking id, a private workflow
#'   symbol/bearer, or a named per-server vector of either form. Tracking ids
#'   use the shared root API; existing private references remain capability
#'   scoped.
#' @return A `dshpc_result` with one status list per site.
#' @examples
#' \dontrun{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' roots <- ds.hpc.list(conns)
#' id <- roots$per_site$site1$tracking_id[[1]]
#' ds.hpc.status(conns["site1"], id)       # shared root, no token
#' ds.hpc.status(conns, private_bearers)    # private compatibility path
#' }
#' @export
ds.hpc.status <- function(conns, job_id) {
  results <- .ds_safe_aggregate(conns,
    expr = function(srv) {
      reference <- .ds_job_reference_for_site(job_id, srv)
      method <- if (.ds_is_tracking_id(as.character(reference))) {
        "hpcTrackingStatusDS"
      } else "hpcStatusDS"
      call(method, reference)
    })
  tracking_sites <- names(results)[vapply(names(results), function(server) {
    reference <- tryCatch(.ds_job_reference_for_site(job_id, server),
      error = function(e) NULL)
    !is.null(reference) && .ds_is_tracking_id(as.character(reference))
  }, logical(1))]
  results <- .ds_transform_site_results(results, .ds_tracking_status,
    sites = tracking_sites)
  errors <- attr(results, "ds_errors") %||% list()
  for (server in intersect(tracking_sites, names(results))) {
    expected <- .ds_tracking_id_for_site(job_id, server)
    if (!identical(results[[server]]$tracking_id, expected)) {
      results[[server]] <- NULL
      errors[[server]] <- "Remote dsHPC request failed."
      warning("dsHPC call failed on server '", server, "'.", call. = FALSE)
    }
  }
  if (length(errors)) attr(results, "ds_errors") <- errors
  dshpc_result(per_site = results)
}

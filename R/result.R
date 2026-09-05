# Module: Job Results

#' Fetch the disclosure-safe dsHPC job result
#'
#' @param conns DSI connections object.
#' @param job_id Character; a public `trk_...` tracking id, a private workflow
#'   symbol/bearer, or a named per-server vector. Shared results require no
#'   token but contain only server-approved disclosure-safe values.
#' @return A `dshpc_result` with one result object per site.
#' @examples
#' \dontrun{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' res <- ds.hpc.result(conns["site1"], "trk_...")
#' str(res$per_site)
#' }
#' @export
ds.hpc.result <- function(conns, job_id) {
  results <- .ds_safe_aggregate(conns,
    expr = function(srv) {
      reference <- .ds_job_reference_for_site(job_id, srv)
      method <- if (.ds_is_tracking_id(as.character(reference))) {
        "hpcTrackingResultDS"
      } else "hpcResultDS"
      call(method, reference)
    })
  tracking_sites <- names(results)[vapply(names(results), function(server) {
    reference <- tryCatch(.ds_job_reference_for_site(job_id, server),
      error = function(e) NULL)
    !is.null(reference) && .ds_is_tracking_id(as.character(reference))
  }, logical(1))]
  results <- .ds_transform_site_results(results, .ds_tracking_result,
    sites = tracking_sites)
  dshpc_result(per_site = results)
}

# Module: Job Status

#' Get dsHPC job status
#'
#' @param conns DSI connections object.
#' @param job_id Character; scalar workflow symbol/bearer, or the named
#'   per-server bearer vector returned by `ds.hpc.job_id()`.
#' @return A `dshpc_result` with one status list per site.
#' @examples
#' \dontrun{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' # job <- dsImagingClient::ds.imaging.qc.metrics(conns, symbol = "jobA")
#' ds.hpc.status(conns, "jobA")                 # by symbol
#' ids <- ds.hpc.job_id(conns, "jobA")
#' ds.hpc.status(conns, ids)                    # by per-site opaque bearers
#' }
#' @export
ds.hpc.status <- function(conns, job_id) {
  results <- .ds_safe_aggregate(conns,
    expr = function(srv) call("hpcStatusDS",
      .ds_job_reference_for_site(job_id, srv)))
  dshpc_result(per_site = results)
}

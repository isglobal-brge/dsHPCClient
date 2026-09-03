# Module: Job Results

#' Fetch the disclosure-safe dsHPC job result
#'
#' @param conns DSI connections object.
#' @param job_id Character; scalar workflow symbol/bearer, or the named
#'   per-server bearer vector returned by `ds.hpc.job_id()`.
#' @return A `dshpc_result` with one result object per site.
#' @examples
#' \dontrun{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' res <- ds.hpc.result(conns, "jobA")
#' str(res$per_site)
#' }
#' @export
ds.hpc.result <- function(conns, job_id) {
  results <- .ds_safe_aggregate(conns,
    expr = function(srv) call("hpcResultDS",
      .ds_job_reference_for_site(job_id, srv)))
  dshpc_result(per_site = results)
}

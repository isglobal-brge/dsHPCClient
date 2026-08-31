# Module: Job Results

#' Fetch the disclosure-safe dsHPC job result
#'
#' @param conns DSI connections object.
#' @param job_id Character; job id or submission symbol (see
#'   `ds.hpc.job_id()`).
#' @return A `dshpc_result` with one result object per site.
#' @examples
#' \donttest{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' res <- ds.hpc.result(conns, "jobA")
#' str(res$per_site)
#' }
#' @export
ds.hpc.result <- function(conns, job_id) {
  results <- list()
  for (srv in names(conns)) {
    r <- DSI::datashield.aggregate(conns[srv],
      expr = call("hpcResultDS", job_id))
    results[[srv]] <- r[[srv]]
  }
  dshpc_result(per_site = results)
}

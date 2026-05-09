# Module: Job Results

#' Fetch the disclosure-safe dsHPC job result
#'
#' @param conns DSI connections object.
#' @param job_id Character job id.
#' @return A `dshpc_result` with one result object per site.
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

# Module: Job Status

#' Get dsHPC job status
#'
#' @param conns DSI connections object.
#' @param job_id Character job id.
#' @return A `dshpc_result` with one status list per site.
#' @export
ds.hpc.status <- function(conns, job_id) {
  results <- list()
  for (srv in names(conns)) {
    r <- DSI::datashield.aggregate(conns[srv],
      expr = call("hpcStatusDS", job_id))
    results[[srv]] <- r[[srv]]
  }
  dshpc_result(per_site = results)
}

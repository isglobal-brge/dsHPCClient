# Module: Job Status

#' Get dsHPC job status
#'
#' @param conns DSI connections object.
#' @param job_id Character; a job id, or the symbol name used in
#'   `datashield.assign()` when submitting (the server resolves the symbol to
#'   its job id; see `ds.hpc.job_id()`).
#' @return A `dshpc_result` with one status list per site.
#' @examples
#' \donttest{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' DSI::datashield.assign(conns, "jobA", call("hpcSubmitDS", spec_enc))
#' ds.hpc.status(conns, "jobA")                 # by symbol
#' ids <- ds.hpc.job_id(conns, "jobA")
#' ds.hpc.status(conns, ids[[1]])               # by job id
#' }
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

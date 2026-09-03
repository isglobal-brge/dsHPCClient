# Module: Job Status

#' Get dsHPC job status
#'
#' @param conns DSI connections object.
#' @param job_id Character; opaque bearer or the symbol assigned by a domain
#'   workflow (see `ds.hpc.job_id()`).
#' @return A `dshpc_result` with one status list per site.
#' @examples
#' \dontrun{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' # job <- dsImagingClient::ds.imaging.qc.metrics(conns, symbol = "jobA")
#' ds.hpc.status(conns, "jobA")                 # by symbol
#' ids <- ds.hpc.job_id(conns, "jobA")
#' ds.hpc.status(conns, ids[[1]])               # by opaque bearer
#' }
#' @export
ds.hpc.status <- function(conns, job_id) {
  results <- list()
  for (srv in names(conns)) {
    r <- .ds_private_aggregate(conns[srv],
      expr = call("hpcStatusDS", job_id))
    results[[srv]] <- r[[srv]]
  }
  dshpc_result(per_site = results)
}

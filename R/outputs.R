# Module: Job Outputs

#' List outputs registered for a dsHPC job
#'
#' @param conns DSI connections object.
#' @param job_id Character job id.
#' @return A `dshpc_result` with one output metadata data.frame per site.
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

#' Get scheduler status
#'
#' Reports backend-agnostic scheduler state for each connected server, including
#' cell/worker health, detected node budget, active usage, and GPU inventory when
#' available.
#'
#' @param conns DSI connections object.
#' @return A dshpc_result with per-site scheduler status lists.
#' @export
ds.hpc.scheduler_status <- function(conns) {
  results <- .ds_safe_aggregate(conns, expr = call("hpcSchedulerStatusDS"))
  dshpc_result(per_site = results)
}

#' Tail sanitized dsHPC job logs
#'
#' @param conns DSI connections object.
#' @param job_id Character job id.
#' @param last_n Integer number of log lines requested from each site.
#' @return A `dshpc_result` with per-site character vectors.
#' @export
ds.hpc.logs <- function(conns, job_id, last_n = 50L) {
  results <- .ds_safe_aggregate(conns,
    expr = call("hpcLogsDS", job_id, as.integer(last_n)))
  dshpc_result(per_site = results)
}

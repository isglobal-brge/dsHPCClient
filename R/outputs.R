# Module: Job Outputs

#' List outputs registered for a dsHPC job
#'
#' @param conns DSI connections object.
#' @param job_id Character; job id or submission symbol (see
#'   `ds.hpc.job_id()`).
#' @return A `dshpc_result` with one output metadata data.frame per site
#'   (columns `name`, `kind`, `safe_for_client`, `size_bytes`); printed as
#'   name/kind/size rows.
#' @examples
#' \donttest{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' ds.hpc.outputs(conns, "jobA")
#' }
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
#' @return A `dshpc_result` whose per-site payload is a named list with
#'   elements:
#'   * `mode`: scheduler mode string.
#'   * `executor`: executor backend status (backend name, availability,
#'     reason when unavailable).
#'   * `cell`: cell/worker health -- `cell_id`, `node_id`, `leader` (lock
#'     holder and heartbeat) and a `workers` data.frame.
#'   * `node`: detected node resource budget (cpu/memory/gpu).
#'   * `usage`: resources currently consumed by running jobs.
#'   * `throttles`: active concurrency throttles.
#'   * `cooldowns`: data.frame of runner cooldowns currently in force.
#' @examples
#' \donttest{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' sch <- ds.hpc.scheduler_status(conns)
#' str(sch$per_site[[1]][c("mode", "executor", "cell")], max.level = 2)
#' }
#' @export
ds.hpc.scheduler_status <- function(conns) {
  results <- .ds_safe_aggregate(conns, expr = call("hpcSchedulerStatusDS"))
  dshpc_result(per_site = results)
}

#' Tail sanitized dsHPC job logs
#'
#' @param conns DSI connections object.
#' @param job_id Character; job id or submission symbol (see
#'   `ds.hpc.job_id()`).
#' @param last_n Integer number of log lines requested from each site.
#' @return A `dshpc_result` with per-site character vectors; printed as
#'   indented log lines.
#' @examples
#' \donttest{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' ds.hpc.logs(conns, "jobA", last_n = 20)
#' }
#' @export
ds.hpc.logs <- function(conns, job_id, last_n = 50L) {
  results <- .ds_safe_aggregate(conns,
    expr = call("hpcLogsDS", job_id, as.integer(last_n)))
  dshpc_result(per_site = results)
}

# Module: Symbol -> Job Id Resolution

#' Resolve the job ids behind a submitted symbol
#'
#' Submission through `DSI::datashield.assign(conns, "jobA",
#' call("hpcSubmitDS", spec_enc))` returns nothing to the analyst: the
#' submission handle (including the job id) lives server-side in the assigned
#' symbol. `ds.hpc.job_id()` resolves the per-server job ids the same way
#' `ds.hpc.status()` does -- the server-side `hpcStatusDS` method accepts a
#' symbol name and reads `job_id` from the assigned object -- so the ids can
#' be reused with `ds.hpc.wait()`, `ds.hpc.result()`, `ds.hpc.outputs()` and
#' `ds.hpc.logs()`.
#'
#' This is the recommended symbol-based submission pattern:
#' 1. `DSI::datashield.assign(conns, "jobA", call("hpcSubmitDS", spec_enc))`
#' 2. `ids <- ds.hpc.job_id(conns, "jobA")`
#' 3. `ds.hpc.wait(conns, ids[["site1"]])` (job ids differ per server).
#'
#' Deduplication semantics: servers deduplicate byte-identical specs against
#' FINISHED/PUBLISHED jobs only; concurrent identical submissions both run.
#'
#' @param conns DSI connections object.
#' @param symbol Character; the symbol used in `datashield.assign()`. A raw
#'   job id also works (it resolves to itself).
#' @return Named character vector mapping server name to job id (`NA` when a
#'   server could not resolve the symbol).
#' @examples
#' \donttest{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' DSI::datashield.assign(conns, "jobA", call("hpcSubmitDS", spec_enc))
#' ids <- ds.hpc.job_id(conns, "jobA")
#' ds.hpc.wait(conns, ids[[1]])
#' }
#' @export
ds.hpc.job_id <- function(conns, symbol) {
  st <- ds.hpc.status(conns, symbol)
  vapply(st$per_site, function(s) {
    if (is.list(s) && !is.null(s$job_id)) as.character(s$job_id)
    else NA_character_
  }, character(1))
}

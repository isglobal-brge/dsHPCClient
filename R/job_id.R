# Module: Symbol -> Job Id Resolution

#' Resolve the job ids behind a submitted symbol
#'
#' Domain packages assign an opaque submission handle to a server-side symbol.
#' `ds.hpc.job_id()` explicitly exports the per-server bearer references through
#' `hpcJobReferenceDS`. Routine status and result calls do not return or retain
#' this transferable credential. References can be reused with `ds.hpc.wait()`,
#' `ds.hpc.result()`, `ds.hpc.outputs()` and `ds.hpc.logs()`.
#'
#' @param conns DSI connections object.
#' @param symbol Character; the symbol assigned by a domain workflow.
#' @return Named character vector mapping server name to an opaque job bearer (`NA` when a
#'   server could not resolve the symbol).
#' @examples
#' \dontrun{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' # job <- dsImagingClient::ds.imaging.qc.metrics(conns, symbol = "jobA")
#' ids <- ds.hpc.job_id(conns, "jobA")
#' ds.hpc.wait(conns, ids)
#' }
#' @export
ds.hpc.job_id <- function(conns, symbol) {
  refs <- lapply(names(conns), function(srv) {
    result <- tryCatch(.ds_private_aggregate(conns[srv],
      expr = call("hpcJobReferenceDS", symbol)), error = function(e) NULL)
    if (is.null(result)) return(NA_character_)
    value <- result[[srv]]
    if (is.character(value) && length(value) == 1L && !is.na(value)) {
      value
    } else {
      NA_character_
    }
  })
  stats::setNames(vapply(refs, identity, character(1)), names(conns))
}

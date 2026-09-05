# Module: Shared Job Summary

#' Summarize shared dsHPC tracking roots
#'
#' Counts logical analyses, never execution children or cohort records. The
#' summary is derived locally from the same fixed root-level schema returned by
#' [ds.hpc.list()].
#'
#' @param conns DSI connections object.
#' @param max_items Optional positive whole-number ceiling per site. The
#'   default retrieves the complete tracking history.
#' @return A `dshpc_result` with `state` and `roots` columns per site.
#' @export
ds.hpc.summary <- function(conns, max_items = Inf) {
  listed <- ds.hpc.list(conns, all = TRUE, max_items = max_items)
  summaries <- lapply(listed$per_site, function(items) {
    counts <- table(factor(items$state, levels = .DSHPC_TRACKING_STATES))
    data.frame(state = .DSHPC_TRACKING_STATES,
      roots = as.integer(counts), stringsAsFactors = FALSE)
  })
  errors <- attr(listed$per_site, "ds_errors")
  if (length(errors)) attr(summaries, "ds_errors") <- errors
  dshpc_result(per_site = summaries, meta = list(scope = "shared_summary"))
}

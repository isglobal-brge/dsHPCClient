# Module: Job Listing

#' Retired analyst job listing
#'
#' Job discovery across workflows is not an analyst operation. Keep the opaque
#' symbol returned by the domain package and call its status method instead.
#'
#' @param conns Ignored compatibility argument.
#' @param label Ignored compatibility argument.
#' @param mode Ignored compatibility argument.
#' @return This function always raises an error before contacting a server.
#' @export
ds.hpc.list <- function(conns, label = NULL, mode = "mine+global") {
  stop(
    "Analyst job listing is retired. Keep the opaque symbol returned by the ",
    "domain package and query that workflow directly.", call. = FALSE)
}

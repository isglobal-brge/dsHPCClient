# Module: Job Summary View

#' Retired cross-workflow job summary
#'
#' @param conns Ignored compatibility argument.
#' @param label Ignored compatibility argument.
#' @return This function always raises the retired-listing error.
#' @export
ds.hpc.summary <- function(conns, label = NULL) {
  stop("ds.hpc.summary() was retired with generic job enumeration; use a domain-specific workflow summary.",
    call. = FALSE)
}

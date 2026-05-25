# Module: Job Listing

#' List submitted dsHPC jobs
#'
#' @param conns DSI connections object.
#' @param label Character or NULL; optional server-side label filter.
#' @param mode Character; "mine", "mine+global", or "global". Reserved for
#'   deployments that expose scoped list policies.
#' @return A `dshpc_result` with one data.frame per site.
#' @export
ds.hpc.list <- function(conns, label = NULL, mode = "mine+global") {
  mode <- match.arg(mode, c("mine", "mine+global", "global"))
  results <- list()
  for (srv in names(conns)) {
    r <- tryCatch({
      backend <- .detect_backend(conns[[srv]])
      scope <- .ds_encode(list(.owner = backend$username))
      if (is.null(label))
        DSI::datashield.aggregate(conns[srv],
          expr = call("hpcListDS", NULL, scope, mode))
      else
        DSI::datashield.aggregate(conns[srv],
          expr = call("hpcListDS", label, scope, mode))
    }, error = function(e) list())
    results[[srv]] <- r[[srv]] %||% .empty_job_list()
  }
  dshpc_result(per_site = results)
}

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
  results <- list()
  for (srv in names(conns)) {
    r <- tryCatch({
      if (is.null(label))
        DSI::datashield.aggregate(conns[srv],
          expr = call("hpcListDS"))
      else
        DSI::datashield.aggregate(conns[srv],
          expr = call("hpcListDS", label))
    }, error = function(e) list())
    results[[srv]] <- r[[srv]] %||% .empty_job_list()
  }
  dshpc_result(per_site = results)
}

# Module: Blocking Wait

#' Wait for a dsHPC job to reach a terminal state
#'
#' @param conns DSI connections object.
#' @param job_id Character; public tracking id, private workflow symbol/bearer,
#'   or a named per-server vector.
#' @param timeout Numeric timeout in seconds.
#' @param poll_interval Numeric polling interval in seconds.
#' @return A `dshpc_result` status object from `ds.hpc.status()`.
#' @examples
#' \dontrun{
#' # conns <- DSI::datashield.login(...)  # live DataSHIELD session
#' ids <- ds.hpc.job_id(conns, "jobA")
#' st <- ds.hpc.wait(conns, ids, timeout = 600, poll_interval = 5)
#' print(st)
#' }
#' @export
ds.hpc.wait <- function(conns, job_id, timeout = 3600, poll_interval = 5) {
  deadline <- Sys.time() + timeout
  srv_names <- names(conns)
  done <- stats::setNames(rep(FALSE, length(srv_names)), srv_names)
  terminal <- c("FINISHED", "PUBLISHED", "FAILED", "CANCELLED", "TERMINAL")
  last <- list()

  # job_id may be a transferable bearer. Never copy it into consoles,
  # notebooks, CI logs, or captured transcripts.
  message("Waiting for job reference ...")
  while (Sys.time() < deadline) {
    for (srv in srv_names[!done]) {
      st <- tryCatch({
        reference <- .ds_job_reference_for_site(job_id, srv)
        shared <- .ds_is_tracking_id(as.character(reference))
        method <- if (shared) "hpcTrackingStatusDS" else "hpcStatusDS"
        r <- .ds_private_aggregate(conns[srv],
          expr = call(method, reference))
        value <- r[[srv]]
        if (shared) {
          status <- .ds_tracking_status(value)
          if (!identical(status$tracking_id, as.character(reference))) {
            stop("Mismatched tracking response.", call. = FALSE)
          }
          status
        } else value
      }, error = function(e) NULL)

      if (is.null(st)) next
      key <- as.character(st$state %||% "UNKNOWN")
      if (!identical(last[[srv]], key)) {
        message("  ", srv, ": ", key)
        last[[srv]] <- key
      }
      if (isTRUE(st$is_done) || toupper(st$state) %in% terminal) {
        done[[srv]] <- TRUE
      }
    }
    if (all(done)) break
    Sys.sleep(poll_interval)
  }
  if (!all(done)) warning("Timeout on: ", paste(srv_names[!done], collapse=", "), call.=FALSE)
  ds.hpc.status(conns, job_id)
}

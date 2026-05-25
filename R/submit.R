# Module: Job Submission
# DS method path: spec is submitted via datashield.assign.expr.

#' Submit a job to all nodes
#'
#' @param conns DSI connections object.
#' @param job A dshpc_job object.
#' @return A dshpc_submission with job_id and per-server details.
ds.hpc.submit <- function(conns, job) {
  .deprecated_domain_api("ds.hpc.submit")
  if (!inherits(job, "dshpc_job"))
    stop("'job' must be a dshpc_job object.", call. = FALSE)

  job_id <- .generate_job_id()

  spec <- as.list(job)
  spec$job_id <- job_id

  submissions <- list()
  for (srv in names(conns)) {
    backend <- .detect_backend(conns[[srv]])
    spec$.owner <- backend$username

    spec_enc <- .ds_encode(spec)
    DSI::datashield.assign.expr(conns[srv], symbol = job_id,
      expr = call("hpcSubmitDS", spec_enc))

    submissions[[srv]] <- list(method = backend$type, username = backend$username)
  }

  result <- list(job_id = job_id,
    name = job$name, label = job$label, visibility = job$visibility,
    servers = names(conns), submissions = submissions,
    submitted_at = Sys.time())
  class(result) <- c("dshpc_submission", "list")
  result
}

#' @export
print.dshpc_submission <- function(x, ...) {
  cat("dshpc_submission\n")
  cat("  Job ID:", x$job_id, "\n")
  if (!is.null(x$name)) cat("  Name:", x$name, "\n")
  if (!is.null(x$label)) cat("  Label:", x$label, "\n")
  cat("  Submitted:", format(x$submitted_at, "%Y-%m-%d %H:%M:%S"), "\n")
  for (srv in names(x$submissions)) {
    s <- x$submissions[[srv]]
    cat("  ", srv, ": ", s$method, " (", s$username, ")\n", sep = "")
  }
  invisible(x)
}

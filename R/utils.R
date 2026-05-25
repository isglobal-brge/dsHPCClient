# Module: Client Utilities

#' Emit the domain-mediated deprecation notice for internal composition helpers
#' @keywords internal
.deprecated_domain_api <- function(name) {
  if (isTRUE(getOption("dshpcclient.silent_deprecation", FALSE)))
    return(invisible(FALSE))
  .Deprecated(
    new = "domain client API",
    package = "dsHPCClient",
    msg = paste0(
      "dsHPCClient::", name, " is deprecated for direct analyst ",
      "use. dsHPCClient is now scoped to queue observability and ",
      "admin operations only. Submission, pipeline composition and ",
      "output loading are server-side concerns mediated by domain ",
      "packages (e.g., dsImagingClient::ds.imaging.* functions)."
    )
  )
}

#' @keywords internal
.generate_job_id <- function() {
  paste0("job_", gsub("-", "", uuid::UUIDgenerate()))
}

#' @keywords internal
.generate_symbol <- function(prefix = "dsH") {
  paste0(prefix, ".",
         paste(sample(c(letters, LETTERS, 0:9), 6, replace = TRUE), collapse = ""))
}

#' @keywords internal
.ds_encode <- function(x) {
  if (is.list(x) || (is.vector(x) && length(x) > 1)) {
    json <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
    b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
    b64 <- gsub("\\+", "-", b64)
    b64 <- gsub("/", "_", b64)
    b64 <- gsub("=+$", "", b64)
    paste0("B64:", b64)
  } else x
}

#' @keywords internal
.ds_safe_aggregate <- function(conns, expr) {
  server_names <- names(conns)
  results <- list()
  errors <- list()
  for (srv in server_names) {
    tryCatch({
      res <- DSI::datashield.aggregate(conns[srv], expr = expr)
      results[[srv]] <- res[[srv]]
    }, error = function(e) { errors[[srv]] <<- e$message })
  }
  if (length(errors) > 0) attr(results, "ds_errors") <- errors
  results
}

#' @keywords internal
.empty_job_list <- function() {
  data.frame(job_id = character(0), state = character(0),
    name = character(0), label = character(0), visibility = character(0),
    owner_id = character(0), submitted_at = character(0),
    progress = character(0), stringsAsFactors = FALSE)
}

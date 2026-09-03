# Module: Client Utilities

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
.ds_private_aggregate <- function(conns, expr) {
  # DSI creates a progress::progress_bar before it checks its own progress
  # option, so both switches are required to keep expressions off consoles.
  old_options <- options(
    datashield.progress = FALSE,
    datashield.errors.print = FALSE,
    progress_enabled = FALSE
  )
  on.exit(options(old_options), add = TRUE)

  tryCatch(
    DSI::datashield.aggregate(conns, expr = expr),
    error = function(e) {
      stop("Remote dsHPC request failed.", call. = FALSE)
    }
  )
}

#' @keywords internal
.ds_safe_aggregate <- function(conns, expr) {
  server_names <- names(conns)
  results <- list()
  errors <- list()
  for (srv in server_names) {
    tryCatch({
      res <- .ds_private_aggregate(conns[srv], expr = expr)
      results[[srv]] <- res[[srv]]
    }, error = function(e) {
      errors[[srv]] <<- "Remote dsHPC request failed."
    })
  }
  # Surface per-server failures at call time; they are also kept in the
  # "ds_errors" attribute, which print.dshpc_result renders.
  for (srv in names(errors)) {
    warning("dsHPC call failed on server '", srv, "'.", call. = FALSE)
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

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

  had_warning <- FALSE
  result <- withCallingHandlers(
    tryCatch(
      DSI::datashield.aggregate(conns, expr = expr),
      error = function(e) {
        stop("Remote dsHPC request failed.", call. = FALSE)
      }
    ),
    warning = function(w) {
      had_warning <<- TRUE
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  if (isTRUE(had_warning)) {
    warning("Remote dsHPC request produced a warning.", call. = FALSE)
  }
  result
}

#' @noRd
.ds_job_reference_for_site <- function(job_id, server) {
  job_names <- names(job_id)
  is_named <- !is.null(job_names) && any(!is.na(job_names) & nzchar(job_names))
  if (is_named) {
    if (anyNA(job_names) || any(!nzchar(job_names)) ||
        anyDuplicated(job_names)) {
      stop("Per-server dsHPC job references must be uniquely named.",
        call. = FALSE)
    }
    if (!server %in% job_names) {
      stop("No dsHPC job reference is available for this server.", call. = FALSE)
    }
    value <- job_id[[server]]
  } else {
    if (length(job_id) != 1L) {
      stop("Per-server dsHPC job references must be named.", call. = FALSE)
    }
    value <- job_id[[1L]]
  }
  if (length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
    stop("No dsHPC job reference is available for this server.", call. = FALSE)
  }
  value
}

#' @keywords internal
.ds_safe_aggregate <- function(conns, expr) {
  server_names <- names(conns)
  results <- list()
  errors <- list()
  for (srv in server_names) {
    tryCatch({
      server_expr <- if (is.function(expr)) expr(srv) else expr
      res <- .ds_private_aggregate(conns[srv], expr = server_expr)
      if (!is.list(res) || is.null(names(res)) ||
          sum(names(res) == srv) != 1L || is.null(res[[srv]])) {
        stop("Remote dsHPC request failed.", call. = FALSE)
      }
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

#' @noRd
.ds_private_assign <- function(conns, symbol, expr, operation = "Assignment") {
  servers <- names(conns)
  if (is.null(servers) || anyNA(servers) || any(!nzchar(servers)) ||
      anyDuplicated(servers)) {
    stop("DataSHIELD connections require non-empty, unique node names.",
      call. = FALSE)
  }
  succeeded <- stats::setNames(rep(FALSE, length(servers)), servers)
  failed <- stats::setNames(rep(FALSE, length(servers)), servers)
  invalid_callback <- FALSE
  success <- function(node, ...) {
    if (length(node) != 1L || is.na(node) || !node %in% servers) {
      invalid_callback <<- TRUE
    } else succeeded[[node]] <<- TRUE
  }
  error <- function(node, ...) {
    if (length(node) != 1L || is.na(node) || !node %in% servers) {
      invalid_callback <<- TRUE
    } else failed[[node]] <<- TRUE
  }

  old_options <- options(datashield.progress = FALSE,
    datashield.errors.print = FALSE, progress_enabled = FALSE)
  on.exit(options(old_options), add = TRUE)
  thrown <- tryCatch({
    withCallingHandlers(
      DSI::datashield.assign.expr(conns, symbol = symbol, expr = expr,
        success = success, error = error, errors.print = FALSE),
      warning = function(w) invokeRestart("muffleWarning"),
      message = function(m) invokeRestart("muffleMessage"))
    NULL
  }, error = identity)

  bad <- servers[!succeeded | failed]
  if (!is.null(thrown) || isTRUE(invalid_callback) || length(bad)) {
    if (!length(bad)) bad <- servers
    stop(operation, " failed or returned no ACK on: ",
      paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

#' @keywords internal
.empty_job_list <- function() {
  data.frame(job_id = character(0), state = character(0),
    name = character(0), label = character(0), visibility = character(0),
    owner_id = character(0), submitted_at = character(0),
    progress = character(0), stringsAsFactors = FALSE)
}

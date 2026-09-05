# Module: Shared Job Tracking

.DSHPC_TRACKING_COLUMNS <- c("tracking_id", "state", "is_done", "kind")
.DSHPC_TRACKING_STATES <- c("queued", "running", "terminal")
.DSHPC_TRACKING_KINDS <- c("analysis", "imaging")
.DSHPC_SHARED_OUTPUT_KINDS <- c("server_object", "summary")
.DSHPC_SHARED_OUTPUT_NAMES <- c("output_001", "output_002")

#' @keywords internal
.ds_tracking_plain_vector <- function(x, type, message) {
  valid <- switch(type,
    character = is.character(x),
    logical = is.logical(x),
    FALSE)
  if (!isTRUE(valid) || !is.null(dim(x))) stop(message, call. = FALSE)
  attributes(x) <- NULL
  x
}

#' @keywords internal
.ds_tracking_has_fields <- function(x, required, exact = FALSE) {
  fields <- names(x)
  is.character(fields) && is.null(attributes(fields)) &&
    !anyNA(fields) && !anyDuplicated(fields) &&
    if (isTRUE(exact)) setequal(fields, required) else all(required %in% fields)
}

#' List shared dsHPC jobs
#'
#' Lists one disclosure-safe tracking root per logical analysis. Execution
#' children, retries, labels, cohort sizes, timestamps, topology, logs and
#' cache information are never requested. A tracking id identifies shared
#' knowledge on the node; it does not identify an execution artifact.
#'
#' @param conns DSI connections object.
#' @param limit Integer page size requested per site, between 1 and 500. This
#'   never limits the historical result when `all = TRUE`.
#' @param cursor Optional scalar cursor, or named per-site cursor vector, from
#'   the preceding page. Cursors contain no job metadata.
#' @param all Logical; follow every page by default. Set to `FALSE` for one
#'   page and read its `next_cursor` and `has_more` attributes.
#' @param max_items Optional positive whole-number ceiling per site when
#'   following pages. The default `Inf` retrieves the complete history.
#' @return A `dshpc_result` containing one root-level data frame per site with
#'   exactly `tracking_id`, `state`, `is_done`, and `kind`. A single-page or
#'   explicitly truncated result carries `next_cursor` and `has_more`
#'   attributes for continuation.
#' @examples
#' \dontrun{
#' jobs <- ds.hpc.list(conns)
#' jobs$per_site$site1
#' }
#' @export
ds.hpc.list <- function(conns, limit = 100L, cursor = NULL, all = TRUE,
                        max_items = Inf) {
  limit <- .ds_tracking_limit(limit)
  if (!is.logical(all) || length(all) != 1L || is.na(all)) {
    stop("'all' must be TRUE or FALSE.", call. = FALSE)
  }
  max_items <- .ds_tracking_max_items(max_items)
  results <- .ds_tracking_collect(conns, limit = limit, cursor = cursor,
    all = all, max_items = max_items)
  dshpc_result(per_site = results, meta = list(scope = "shared_tracking"))
}

#' @keywords internal
.ds_tracking_limit <- function(limit) {
  if (length(limit) != 1L || is.na(limit) || !is.numeric(limit) ||
      !is.finite(limit) || limit != floor(limit) || limit < 1L ||
      limit > 500L) {
    stop("'limit' must be one whole number between 1 and 500.",
      call. = FALSE)
  }
  as.integer(limit)
}

#' @keywords internal
.ds_tracking_max_items <- function(max_items) {
  if (length(max_items) != 1L || is.na(max_items) || !is.numeric(max_items) ||
      max_items <= 0 || (is.finite(max_items) && max_items != floor(max_items))) {
    stop("'max_items' must be a positive whole number or Inf.",
      call. = FALSE)
  }
  as.numeric(max_items)
}

#' @keywords internal
.ds_tracking_collect <- function(conns, limit, cursor, all, max_items) {
  servers <- names(conns)
  if (is.null(servers) || anyNA(servers) || any(!nzchar(servers)) ||
      anyDuplicated(servers)) {
    stop("DataSHIELD connections require unique non-empty node names.",
      call. = FALSE)
  }
  cursors <- stats::setNames(lapply(servers, function(server)
    .ds_tracking_cursor_for_site(cursor, server)), servers)
  collected <- stats::setNames(lapply(servers, function(server)
    .ds_tracking_table(data.frame(
      tracking_id = character(0), state = character(0),
      is_done = logical(0), kind = character(0),
      stringsAsFactors = FALSE))), servers)
  active <- servers
  errors <- list()
  seen <- stats::setNames(lapply(servers, function(server) {
    value <- cursors[[server]]
    if (is.null(value)) character(0) else value
  }), servers)

  while (length(active)) {
    page_limits <- stats::setNames(vapply(active, function(server) {
      remaining <- max_items - nrow(collected[[server]])
      if (is.finite(remaining)) as.integer(min(limit, remaining)) else limit
    }, integer(1)), active)
    page_result <- .ds_safe_aggregate(conns[active],
      expr = function(server) {
        call("hpcTrackingListDS", page_limits[[server]], cursors[[server]])
      })
    page_errors <- attr(page_result, "ds_errors") %||% list()
    errors[names(page_errors)] <- page_errors
    next_active <- character(0)

    for (server in active) {
      if (!server %in% names(page_result)) next
      page <- tryCatch(.ds_tracking_page(page_result[[server]],
        expected_limit = page_limits[[server]]),
        error = function(e) NULL)
      if (is.null(page)) {
        errors[[server]] <- "Remote dsHPC request failed."
        warning("dsHPC call failed on server '", server, "'.", call. = FALSE)
        next
      }
      if (isTRUE(page$has_more) && page$next_cursor %in% seen[[server]]) {
        errors[[server]] <- "Remote dsHPC request failed."
        warning("dsHPC call failed on server '", server, "'.",
          call. = FALSE)
        next
      }
      take <- nrow(page$items)
      if (take > 0L) {
        combined <- rbind(collected[[server]],
          page$items[seq_len(take), , drop = FALSE])
        if (anyDuplicated(combined$tracking_id)) {
          errors[[server]] <- "Remote dsHPC request failed."
          warning("dsHPC call failed on server '", server, "'.",
            call. = FALSE)
          next
        }
        collected[[server]] <- combined
      }
      reached_max <- is.finite(max_items) &&
        nrow(collected[[server]]) >= max_items
      continue <- isTRUE(all) && isTRUE(page$has_more) && !reached_max
      if (continue) {
        seen[[server]] <- c(seen[[server]], page$next_cursor)
        cursors[[server]] <- page$next_cursor
        next_active <- c(next_active, server)
      } else {
        attr(collected[[server]], "has_more") <- isTRUE(page$has_more)
        attr(collected[[server]], "next_cursor") <- page$next_cursor
        attr(collected[[server]], "schema") <- "root_v1"
      }
    }
    active <- unique(next_active)
  }

  if (length(errors)) attr(collected, "ds_errors") <- errors
  collected
}

#' @keywords internal
.ds_tracking_cursor_for_site <- function(cursor, server) {
  if (is.null(cursor)) return(NULL)
  value <- .ds_job_reference_for_site(cursor, server)
  value <- .ds_tracking_plain_vector(value, "character",
    "A valid shared tracking cursor is required.")
  if (length(value) != 1L || is.na(value) ||
      !grepl(paste0("^cur_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-",
        "[89ab][0-9a-f]{3}-[0-9a-f]{12}$"), value)) {
    stop("A valid shared tracking cursor is required.", call. = FALSE)
  }
  value
}

#' @keywords internal
.ds_is_tracking_id <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl(paste0("^trk_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-",
      "[89ab][0-9a-f]{3}-[0-9a-f]{12}$"), x)
}

#' @keywords internal
.ds_tracking_id_for_site <- function(tracking_id, server) {
  value <- .ds_job_reference_for_site(tracking_id, server)
  value <- .ds_tracking_plain_vector(value, "character",
    "A valid shared dsHPC tracking id is required.")
  if (!.ds_is_tracking_id(value)) {
    stop("A valid shared dsHPC tracking id is required.", call. = FALSE)
  }
  value
}

#' @keywords internal
.ds_tracking_table <- function(x) {
  if (!is.data.frame(x) ||
      !.ds_tracking_has_fields(x, .DSHPC_TRACKING_COLUMNS)) {
    stop("Invalid shared tracking response.", call. = FALSE)
  }
  values <- x[, .DSHPC_TRACKING_COLUMNS, drop = FALSE]
  tracking_id <- .ds_tracking_plain_vector(values$tracking_id, "character",
    "Invalid shared tracking response.")
  state <- .ds_tracking_plain_vector(values$state, "character",
    "Invalid shared tracking response.")
  is_done <- .ds_tracking_plain_vector(values$is_done, "logical",
    "Invalid shared tracking response.")
  kind <- .ds_tracking_plain_vector(values$kind, "character",
    "Invalid shared tracking response.")
  out <- data.frame(tracking_id = tracking_id, state = state,
    is_done = is_done, kind = kind, stringsAsFactors = FALSE)
  valid <- is.logical(out$is_done) && !anyNA(out) &&
    all(vapply(out$tracking_id, .ds_is_tracking_id, logical(1))) &&
    all(out$state %in% .DSHPC_TRACKING_STATES) &&
    all(out$kind %in% .DSHPC_TRACKING_KINDS) &&
    all(out$is_done == (out$state == "terminal")) &&
    !anyDuplicated(out$tracking_id)
  if (!isTRUE(valid)) {
    stop("Invalid shared tracking response.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

#' @keywords internal
.ds_tracking_page <- function(x, expected_limit = NULL) {
  required <- c("items", "next_cursor", "has_more", "schema")
  if (!is.list(x) || is.object(x) ||
      !.ds_tracking_has_fields(x, required, exact = TRUE) ||
      !identical(x[["schema", exact = TRUE]], "root_v1") ||
      length(x[["has_more", exact = TRUE]]) != 1L ||
      !is.logical(x[["has_more", exact = TRUE]]) ||
      is.na(x[["has_more", exact = TRUE]])) {
    stop("Invalid shared tracking response.", call. = FALSE)
  }
  items <- .ds_tracking_table(x[["items", exact = TRUE]])
  if (!is.null(expected_limit) &&
      (length(expected_limit) != 1L || !is.numeric(expected_limit) ||
       is.na(expected_limit) || !is.finite(expected_limit) ||
       expected_limit != floor(expected_limit) || expected_limit < 1L ||
       nrow(items) > expected_limit)) {
    stop("Invalid shared tracking response.", call. = FALSE)
  }
  if (isTRUE(x[["has_more", exact = TRUE]]) && nrow(items) == 0L) {
    stop("Invalid shared tracking response.", call. = FALSE)
  }
  next_cursor <- x[["next_cursor", exact = TRUE]]
  if (isTRUE(x[["has_more", exact = TRUE]])) {
    next_cursor <- .ds_tracking_plain_vector(next_cursor, "character",
      "Invalid shared tracking response.")
    if (length(next_cursor) != 1L || is.na(next_cursor) ||
        !grepl(paste0("^cur_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-",
          "[89ab][0-9a-f]{3}-[0-9a-f]{12}$"), next_cursor)) {
      stop("Invalid shared tracking response.", call. = FALSE)
    }
  } else {
    if (!is.null(next_cursor)) {
      stop("Invalid shared tracking response.", call. = FALSE)
    }
    next_cursor <- NULL
  }
  list(items = items, next_cursor = next_cursor,
    has_more = isTRUE(x[["has_more", exact = TRUE]]), schema = "root_v1")
}

#' @keywords internal
.ds_tracking_status <- function(x) {
  if (!is.list(x) || is.object(x) ||
      !.ds_tracking_has_fields(x, .DSHPC_TRACKING_COLUMNS)) {
    stop("Invalid shared tracking response.", call. = FALSE)
  }
  if (any(vapply(x[.DSHPC_TRACKING_COLUMNS], length, integer(1)) != 1L)) {
    stop("Invalid shared tracking response.", call. = FALSE)
  }
  if (!is.logical(x$is_done) || is.na(x$is_done)) {
    stop("Invalid shared tracking response.", call. = FALSE)
  }
  row <- data.frame(
    tracking_id = .ds_tracking_plain_vector(x$tracking_id, "character",
      "Invalid shared tracking response."),
    state = .ds_tracking_plain_vector(x$state, "character",
      "Invalid shared tracking response."),
    is_done = .ds_tracking_plain_vector(x$is_done, "logical",
      "Invalid shared tracking response."),
    kind = .ds_tracking_plain_vector(x$kind, "character",
      "Invalid shared tracking response."),
    stringsAsFactors = FALSE
  )
  row <- .ds_tracking_table(row)
  as.list(row[1L, , drop = FALSE])
}

#' @keywords internal
.ds_tracking_outputs <- function(x) {
  columns <- c("name", "kind", "classification")
  if (!is.data.frame(x) || !.ds_tracking_has_fields(x, columns)) {
    stop("Invalid shared output response.", call. = FALSE)
  }
  values <- x[, columns, drop = FALSE]
  out <- data.frame(
    name = .ds_tracking_plain_vector(values$name, "character",
      "Invalid shared output response."),
    kind = .ds_tracking_plain_vector(values$kind, "character",
      "Invalid shared output response."),
    classification = .ds_tracking_plain_vector(values$classification,
      "character", "Invalid shared output response."),
    stringsAsFactors = FALSE)
  valid_pair <- (out$kind == "summary" &
    out$classification == "client_safe" & out$name == "output_002") |
    (out$kind == "server_object" &
      out$classification == "server_reusable" & out$name == "output_001")
  if (anyNA(out) || nrow(out) > 2L ||
      !all(out$name %in% .DSHPC_SHARED_OUTPUT_NAMES) ||
      !all(out$kind %in% .DSHPC_SHARED_OUTPUT_KINDS) ||
      !all(valid_pair) || anyDuplicated(out$name) || anyDuplicated(out$kind)) {
    stop("Invalid shared output response.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

#' @keywords internal
.ds_tracking_summary_value <- function(value) {
  if (!is.list(value) || is.object(value)) return(FALSE)
  attrs <- attributes(value)
  if (!is.null(attrs) && !identical(names(attrs), "names")) return(FALSE)
  if (length(value) == 0L) return(is.null(names(value)))
  if (is.null(names(value)) || !is.null(attributes(names(value))) ||
      anyDuplicated(names(value)) ||
      !all(names(value) %in% c("n_output_files", "n_samples"))) {
    return(FALSE)
  }
  all(vapply(value, function(x) {
    (is.integer(x) || is.numeric(x)) && length(x) == 1L &&
      is.null(attributes(x)) &&
      ((is.na(x) && !is.nan(x)) ||
       (!is.na(x) && is.finite(x) && x >= 3 && x == floor(x)))
  }, logical(1)))
}

#' @keywords internal
.ds_tracking_result <- function(x) {
  if (!is.list(x) || is.object(x) ||
      !.ds_tracking_has_fields(x, "ready") ||
      length(x[["ready", exact = TRUE]]) != 1L ||
      is.na(x[["ready", exact = TRUE]]) ||
      !is.logical(x[["ready", exact = TRUE]])) {
    stop("Invalid shared result response.", call. = FALSE)
  }
  if (!isTRUE(x[["ready", exact = TRUE]])) {
    if (!.ds_tracking_has_fields(x, c("ready", "state", "error"))) {
      stop("Invalid shared result response.", call. = FALSE)
    }
    state <- .ds_tracking_plain_vector(x[["state", exact = TRUE]], "character",
      "Invalid shared result response.")
    error <- .ds_tracking_plain_vector(x[["error", exact = TRUE]], "character",
      "Invalid shared result response.")
    if (length(state) != 1L || is.na(state) ||
        !state %in% .DSHPC_TRACKING_STATES ||
        length(error) != 1L ||
        (identical(state, "terminal") &&
         !identical(error, "Job execution failed.")) ||
        (!identical(state, "terminal") && !is.na(error))) {
      stop("Invalid shared result response.", call. = FALSE)
    }
    return(list(state = state, ready = FALSE, error = error))
  }
  if (!.ds_tracking_has_fields(x,
      c("ready", "summaries", "available_outputs"))) {
    stop("Invalid shared result response.", call. = FALSE)
  }
  out <- list(ready = TRUE,
    summaries = x[["summaries", exact = TRUE]],
    available_outputs = x[["available_outputs", exact = TRUE]])
  if (is.null(out$summaries)) out$summaries <- list()
  if (is.null(out$available_outputs)) out$available_outputs <- list()
  if (!is.list(out$summaries) || is.object(out$summaries) ||
      !is.list(out$available_outputs) || is.object(out$available_outputs)) {
    stop("Invalid shared result response.", call. = FALSE)
  }
  if (length(out$summaries) > 1L || length(out$available_outputs) > 1L) {
    stop("Invalid shared result response.", call. = FALSE)
  }
  out$summaries <- unname(lapply(out$summaries, function(item) {
    if (!is.list(item) || is.object(item) ||
        !.ds_tracking_has_fields(item, c("name", "kind"))) {
      stop("Invalid shared result response.", call. = FALSE)
    }
    name <- .ds_tracking_plain_vector(item$name, "character",
      "Invalid shared result response.")
    kind <- .ds_tracking_plain_vector(item$kind, "character",
      "Invalid shared result response.")
    if (!identical(name, "output_002") ||
        !identical(kind, "summary") ||
        ("value" %in% names(item) &&
         !.ds_tracking_summary_value(item$value))) {
      stop("Invalid shared result response.", call. = FALSE)
    }
    value <- list(name = name, kind = kind)
    if ("value" %in% names(item)) value$value <- item$value
    value
  }))
  out$available_outputs <- unname(lapply(out$available_outputs, function(item) {
    if (!is.list(item) || is.object(item) ||
        !.ds_tracking_has_fields(item, c("name", "kind"))) {
      stop("Invalid shared result response.", call. = FALSE)
    }
    name <- .ds_tracking_plain_vector(item$name, "character",
      "Invalid shared result response.")
    kind <- .ds_tracking_plain_vector(item$kind, "character",
      "Invalid shared result response.")
    if (!identical(name, "output_002") || !identical(kind, "summary")) {
      stop("Invalid shared result response.", call. = FALSE)
    }
    list(name = name, kind = kind)
  }))
  if (length(out$summaries) != length(out$available_outputs) ||
      (length(out$summaries) == 1L &&
       (!identical(out$summaries[[1L]]$name,
          out$available_outputs[[1L]]$name) ||
        !identical(out$summaries[[1L]]$kind,
          out$available_outputs[[1L]]$kind)))) {
    stop("Invalid shared result response.", call. = FALSE)
  }
  out
}

#' @keywords internal
.ds_transform_site_results <- function(results, transform, sites = names(results)) {
  errors <- attr(results, "ds_errors") %||% list()
  for (server in intersect(sites, names(results))) {
    value <- tryCatch(transform(results[[server]]), error = function(e) NULL)
    if (is.null(value)) {
      results[[server]] <- NULL
      errors[[server]] <- "Remote dsHPC request failed."
      warning("dsHPC call failed on server '", server, "'.", call. = FALSE)
    } else {
      results[[server]] <- value
    }
  }
  if (length(errors)) attr(results, "ds_errors") <- errors
  results
}

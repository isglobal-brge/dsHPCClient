# Module: Result Objects

#' @keywords internal
dshpc_result <- function(per_site, pooled = NULL, meta = list()) {
  obj <- list(per_site = per_site, pooled = pooled,
    meta = list(timestamp = Sys.time(), servers = names(per_site),
                scope = meta$scope %||% "per_site"))
  class(obj) <- c("dshpc_result", "list")
  obj
}
#' @export
print.dshpc_result <- function(x, ...) {
  cat("dshpc_result\n  Servers:", paste(x$meta$servers, collapse=", "), "\n")
  for (srv in x$meta$servers) {
    .print_site_payload(srv, x$per_site[[srv]])
  }
  errs <- attr(x$per_site, "ds_errors")
  if (length(errs) > 0) {
    cat("  Errors:\n")
    for (srv in names(errs))
      cat("    ", srv, ": ", errs[[srv]], "\n", sep = "")
  }
  invisible(x)
}

#' Render one server's payload according to its shape
#'
#' Dispatches on payload shape: jobs data.frame -> formatted table,
#' outputs data.frame -> name/kind/size rows, character vector (logs) ->
#' indented lines, status-like list -> one state line, otherwise a compact
#' fallback.
#' @keywords internal
.print_site_payload <- function(srv, site) {
  # Jobs listing (hpcListDS / hpcAdminListDS payloads)
  if (is.data.frame(site) && all(c("job_id", "state") %in% names(site))) {
    if (nrow(site) == 0) {
      cat("  ", srv, ": (no jobs)\n", sep = "")
    } else {
      cat("  ", srv, ": ", nrow(site), " job(s)\n", sep = "")
      .print_indented_df(site)
    }
    return(invisible(NULL))
  }

  # Outputs listing (hpcOutputsDS payload)
  if (is.data.frame(site) && all(c("name", "kind") %in% names(site))) {
    if (nrow(site) == 0) {
      cat("  ", srv, ": (no outputs)\n", sep = "")
    } else {
      cat("  ", srv, ": ", nrow(site), " output(s)\n", sep = "")
      for (i in seq_len(nrow(site))) {
        cat(sprintf("    %s  [%s]  %s\n", site$name[i], site$kind[i],
                    .fmt_bytes(site$size_bytes[i] %||% NA)))
      }
    }
    return(invisible(NULL))
  }

  # Log lines (hpcLogsDS payload) or any plain character vector
  if (is.character(site)) {
    if (length(site) == 0) {
      cat("  ", srv, ": (no log lines)\n", sep = "")
    } else {
      cat("  ", srv, ": ", length(site), " log line(s)\n", sep = "")
      cat(paste0("    ", site, collapse = "\n"), "\n", sep = "")
    }
    return(invisible(NULL))
  }

  # Status-like list (hpcStatusDS payload)
  if (is.list(site) && !is.null(site$state)) {
    cat("  ", srv, ": ", site$state, if (!is.null(site$step_index))
      paste0(" [", site$step_index, "/", site$total_steps %||% "?", "]") else "",
      "\n", sep = "")
    return(invisible(NULL))
  }

  # Scalar / other payloads: compact fallback
  if (is.null(site) || length(site) == 0) {
    cat("  ", srv, ": (empty)\n", sep = "")
  } else if (is.atomic(site) && length(site) == 1) {
    cat("  ", srv, ": ", format(site), "\n", sep = "")
  } else {
    cat("  ", srv, ": <", class(site)[1], "> with ", length(site),
        " element(s)\n", sep = "")
  }
  invisible(NULL)
}

#' @keywords internal
.print_indented_df <- function(df, indent = "    ") {
  txt <- utils::capture.output(print(df, row.names = FALSE))
  cat(paste0(indent, txt, collapse = "\n"), "\n", sep = "")
}

#' @keywords internal
.fmt_bytes <- function(n) {
  n <- suppressWarnings(as.numeric(n))
  if (is.na(n)) return("size unknown")
  if (n < 1024) return(paste0(n, " B"))
  if (n < 1024^2) return(sprintf("%.1f KB", n / 1024))
  if (n < 1024^3) return(sprintf("%.1f MB", n / 1024^2))
  sprintf("%.1f GB", n / 1024^3)
}
#' @export
`$.dshpc_result` <- function(x, name) {
  if (name %in% c("per_site","pooled","meta")) return(.subset2(x, name))
  ps <- .subset2(x, "per_site")
  if (name %in% names(ps)) return(ps[[name]])
  .subset2(x, name)
}
#' @export
as.data.frame.dshpc_result <- function(x, ...) {
  if (!is.null(x$pooled) && is.data.frame(x$pooled)) return(x$pooled)
  ps <- x$per_site
  if (length(ps) > 0 && is.data.frame(ps[[1]])) return(ps[[1]])
  data.frame()
}

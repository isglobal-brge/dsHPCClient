# Module: disclosure-safe dsHPC Studio

#' Launch dsHPC Studio
#'
#' Opens a local Shiny browser for shared logical analyses. Studio uses only
#' the versioned root-tracking API and never requests execution children,
#' labels, timestamps, progress counts, logs, scheduler topology or cache data.
#' Reusable outputs can be assigned as opaque references in the selected server
#' session without downloading their values.
#'
#' @param conns DSI connections object.
#' @param host Character host passed to `shiny::runApp()`.
#' @param port Optional port passed to `shiny::runApp()`.
#' @param launch.browser Logical; passed to `shiny::runApp()`.
#' @return The return value from `shiny::runApp()`.
#' @export
ds.hpc.studio <- function(conns, host = "127.0.0.1", port = NULL,
                          launch.browser = interactive()) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to launch dsHPC Studio.",
      call. = FALSE)
  }
  conns <- .studio_named_conns(conns)
  app <- .studio_app(conns)
  args <- list(appDir = app, host = host, launch.browser = launch.browser)
  if (!is.null(port)) args$port <- port
  do.call(shiny::runApp, args)
}

#' Fetch disclosure-safe Studio data
#'
#' Fetches one page of shared tracking roots from each selected server. Use the
#' returned `next_cursor` with another call, or launch [ds.hpc.studio()] and use
#' its **Load more** action. A tracking id can always be looked up directly.
#'
#' @param conns DSI connections object.
#' @param server Optional server name; all servers are queried by default.
#' @param limit Integer page size from 1 to 500.
#' @param cursor Optional scalar or named per-site cursor.
#' @return A `dshpc_result` with one safe Studio snapshot per site.
#' @export
ds.hpc.studio_data <- function(conns, server = NULL, limit = 100L,
                               cursor = NULL) {
  conns <- .studio_named_conns(conns)
  limit <- .ds_tracking_limit(limit)
  servers <- server %||% names(conns)
  bad <- setdiff(servers, names(conns))
  if (length(bad)) stop("Unknown server: ", bad[1L], call. = FALSE)
  out <- stats::setNames(lapply(servers, function(site) {
    site_cursor <- tryCatch(.ds_tracking_cursor_for_site(cursor, site),
      error = function(e) e)
    if (inherits(site_cursor, "error")) stop(conditionMessage(site_cursor),
      call. = FALSE)
    .studio_fetch_one(conns, site, limit = limit, cursor = site_cursor)
  }), servers)
  dshpc_result(per_site = out, meta = list(scope = "studio"))
}

#' @keywords internal
.studio_named_conns <- function(conns) {
  if (inherits(conns, "DSConnection")) {
    name <- tryCatch(conns@name, error = function(e) NULL)
    if (!is.character(name) || length(name) != 1L || is.na(name) ||
        !nzchar(name)) name <- "server_1"
    return(stats::setNames(list(conns), name))
  }
  if (!is.list(conns) || !length(conns)) {
    stop("At least one DataSHIELD connection is required.", call. = FALSE)
  }
  n <- names(conns)
  if (is.null(n) || length(n) != length(conns)) n <- rep("", length(conns))
  missing <- is.na(n) | !nzchar(n)
  if (any(missing)) n[missing] <- paste0("server_", which(missing))
  if (anyDuplicated(n)) {
    stop("DataSHIELD connections require unique node names.", call. = FALSE)
  }
  names(conns) <- n
  conns
}

#' @keywords internal
.studio_response_for_server <- function(response, server) {
  if (!is.list(response)) {
    stop("Invalid Studio response.", call. = FALSE)
  }
  response_names <- names(response)
  if (is.null(response_names)) {
    if (length(response) != 1L || is.null(response[[1L]])) {
      stop("Invalid Studio response.", call. = FALSE)
    }
    return(response[[1L]])
  }
  if (length(response) != 1L || anyNA(response_names) ||
      any(!nzchar(response_names)) || !identical(response_names, server) ||
      is.null(response[[server]])) {
    stop("Invalid Studio response.", call. = FALSE)
  }
  response[[server]]
}

#' @keywords internal
.studio_capabilities <- function(conns, server) {
  response <- .ds_private_aggregate(conns[server],
    expr = call("hpcCapabilitiesDS"))
  value <- .studio_response_for_server(response, server)
  fields <- c("shared_tracking", "shared_results", "reusable_outputs",
    "queue_visibility")
  if (!is.list(value) || is.object(value) ||
      !.ds_tracking_has_fields(value, fields)) return(NULL)
  required <- identical(value[["shared_tracking", exact = TRUE]], "root_v1") &&
    identical(value[["shared_results", exact = TRUE]], "safe_v1") &&
    identical(value[["reusable_outputs", exact = TRUE]], "opaque_ref_v1")
  if (!required) return(NULL)
  visibility <- value[["queue_visibility", exact = TRUE]]
  if (!is.character(visibility) || !is.null(attributes(visibility)) ||
      length(visibility) != 1L || is.na(visibility) ||
      !visibility %in% c("shared", "scoped")) return(NULL)
  list(shared_tracking = "root_v1", shared_results = "safe_v1",
    reusable_outputs = "opaque_ref_v1", queue_visibility = visibility)
}

#' @keywords internal
.studio_fetch_one <- function(conns, server, limit = 100L, cursor = NULL) {
  tryCatch({
    capabilities <- .studio_capabilities(conns, server)
    if (is.null(capabilities)) {
      return(.studio_error_snapshot(server,
        "Shared dsHPC tracking is unavailable on this server."))
    }
    if (!identical(capabilities$queue_visibility, "shared")) {
      return(.studio_error_snapshot(server,
        "The shared dsHPC queue is disabled on this server."))
    }
    response <- .ds_private_aggregate(conns[server],
      expr = call("hpcTrackingListDS", as.integer(limit), cursor))
    page <- .ds_tracking_page(.studio_response_for_server(response, server),
      expected_limit = limit)
    if (!is.null(cursor) && isTRUE(page$has_more) &&
        identical(page$next_cursor, cursor)) {
      stop("Replayed Studio cursor.", call. = FALSE)
    }
    list(ok = TRUE, error = NULL, server = server,
      jobs = page$items, next_cursor = page$next_cursor,
      has_more = page$has_more, schema = "root_v1",
      queue_visibility = "shared")
  }, error = function(e) {
    .studio_error_snapshot(server, "Shared dsHPC tracking is unavailable.")
  })
}

#' @keywords internal
.studio_fetch_detail <- function(conns, server, tracking_id) {
  tryCatch({
    tracking_id <- .ds_tracking_id_for_site(tracking_id, server)
    aggregate_one <- function(method) {
      response <- .ds_private_aggregate(conns[server],
        expr = call(method, tracking_id))
      .studio_response_for_server(response, server)
    }
    status <- .ds_tracking_status(aggregate_one("hpcTrackingStatusDS"))
    if (!identical(status$tracking_id, tracking_id)) {
      stop("Mismatched tracking response.", call. = FALSE)
    }
    outputs <- .ds_tracking_outputs(aggregate_one("hpcTrackingOutputsDS"))
    result <- .ds_tracking_result(aggregate_one("hpcTrackingResultDS"))
    list(ok = TRUE, error = NULL, status = status, outputs = outputs,
      result = result)
  }, error = function(e) {
    list(ok = FALSE, error = "Shared job details are unavailable.",
      status = NULL, outputs = .studio_empty_outputs(), result = NULL)
  })
}

#' @keywords internal
.studio_assign_one <- function(conns, server, tracking_id, output_name,
                               symbol) {
  tryCatch({
    ds.hpc.load_output(conns[server], tracking_id, output_name,
      symbol = symbol)
    list(ok = TRUE, error = NULL)
  }, error = function(e) {
    list(ok = FALSE, error = "Shared output assignment failed.")
  })
}

#' @keywords internal
.studio_error_snapshot <- function(server, error) {
  list(ok = FALSE, error = error, server = server,
    jobs = .studio_empty_jobs(), next_cursor = NULL, has_more = FALSE,
    schema = "root_v1", queue_visibility = NULL)
}

#' @keywords internal
.studio_empty_jobs <- function() {
  data.frame(tracking_id = character(0), state = character(0),
    is_done = logical(0), kind = character(0), stringsAsFactors = FALSE)
}

#' @keywords internal
.studio_empty_outputs <- function() {
  data.frame(name = character(0), kind = character(0),
    classification = character(0), stringsAsFactors = FALSE)
}

#' @keywords internal
.studio_app <- function(conns) {
  shiny::shinyApp(
    ui = .studio_ui(conns),
    server = function(input, output, session) {
      snapshot <- shiny::reactiveVal(NULL)
      seen_cursors <- shiny::reactiveVal(character(0))
      selected <- shiny::reactiveVal(NULL)
      detail <- shiny::reactiveVal(NULL)
      assignment <- shiny::reactiveVal(NULL)

      fetch <- function(append = FALSE) {
        server <- input$server %||% names(conns)[1L]
        current <- snapshot()
        cursor <- if (append && !is.null(current)) current$next_cursor else NULL
        page <- .studio_fetch_one(conns, server, cursor = cursor)
        history <- if (append) seen_cursors() else character(0)
        merged <- .studio_merge_page(current, page, append = append,
          requested_cursor = cursor, seen_cursors = history)
        if (append && is.null(merged$page_error) && !is.null(cursor)) {
          seen_cursors(unique(c(history, cursor)))
        } else if (!append) {
          seen_cursors(character(0))
        }
        snapshot(merged)
        if (!append) {
          selected(NULL)
          detail(NULL)
          assignment(NULL)
        }
      }

      shiny::observeEvent(input$server, fetch(FALSE), ignoreInit = FALSE)
      shiny::observeEvent(input$refresh, fetch(FALSE), ignoreInit = TRUE)
      shiny::observeEvent(input$load_more, fetch(TRUE), ignoreInit = TRUE)

      filtered <- shiny::reactive({
        snap <- snapshot()
        if (is.null(snap) || !isTRUE(snap$ok)) return(.studio_empty_jobs())
        .studio_filter_jobs_by_state(snap$jobs,
          input$state_filter %||% "all")
      })

      shiny::observeEvent(input$selected_tracking_id, {
        selected(input$selected_tracking_id)
        assignment(NULL)
      })

      shiny::observeEvent(input$lookup, {
        id <- trimws(input$lookup_id %||% "")
        if (!.ds_is_tracking_id(id)) {
          detail(list(ok = FALSE,
            error = "Enter a valid shared tracking ID.",
            status = NULL, outputs = .studio_empty_outputs(), result = NULL))
          return()
        }
        selected(id)
        assignment(NULL)
      })

      shiny::observeEvent(list(selected(), input$server), {
        id <- selected()
        if (is.null(id)) {
          detail(NULL)
        } else {
          detail(.studio_fetch_detail(conns, input$server, id))
        }
      }, ignoreInit = TRUE)

      output$status_bar <- shiny::renderUI({
        snap <- snapshot()
        if (is.null(snap)) return(shiny::div(class = "status-line muted",
          "Waiting for shared tracking"))
        if (!isTRUE(snap$ok)) return(shiny::div(class = "status-line error",
          snap$error))
        if (!is.null(snap$page_error)) {
          return(shiny::div(class = "status-line error", snap$page_error))
        }
        shiny::div(class = "status-line ok",
          "Shared root tracking is available. Data and execution details remain server-side.")
      })

      output$job_list <- shiny::renderUI({
        .studio_job_list(filtered(), selected())
      })

      output$more_button <- shiny::renderUI({
        snap <- snapshot()
        if (is.null(snap) || !isTRUE(snap$ok) || !isTRUE(snap$has_more)) {
          return(NULL)
        }
        shiny::actionButton("load_more", "Load more")
      })

      output$job_detail <- shiny::renderUI({
        value <- detail()
        if (!is.null(value) && !isTRUE(value$ok)) {
          return(shiny::div(class = "status-line error", value$error))
        }
        if (is.null(selected())) return(shiny::div(class = "empty-state",
          "Select an analysis or find it by tracking ID."))
        if (is.null(value)) return(shiny::div(class = "empty-state",
          "Loading shared analysis..."))
        .studio_job_detail(value$status)
      })

      output$outputs_table <- shiny::renderTable({
        value <- detail()
        if (is.null(value) || !isTRUE(value$ok)) return(data.frame())
        .studio_outputs_table(value$outputs)
      }, striped = TRUE, bordered = FALSE, spacing = "s", rownames = FALSE)

      output$output_loader <- shiny::renderUI({
        value <- detail()
        if (is.null(value) || !isTRUE(value$ok) ||
            !nrow(value$outputs)) return(NULL)
        choices <- value$outputs$name
        shiny::div(class = "output-loader",
          shiny::selectInput("output_name", "Output", choices = choices,
            selected = choices[1L], width = "220px"),
          shiny::textInput("output_symbol", "Server symbol",
            value = .ds_default_output_symbol(choices[1L]), width = "220px"),
          shiny::actionButton("assign_output", "Load server-side",
            class = "btn-primary"))
      })

      shiny::observeEvent(input$assign_output, {
        id <- selected()
        if (is.null(id)) return()
        assignment(.studio_assign_one(conns, input$server, id,
          input$output_name, input$output_symbol))
      }, ignoreInit = TRUE)

      output$assignment_status <- shiny::renderUI({
        value <- assignment()
        if (is.null(value)) return(NULL)
        if (isTRUE(value$ok)) {
          shiny::div(class = "status-line ok",
            "Opaque output reference assigned in the server session.")
        } else {
          shiny::div(class = "status-line error", value$error)
        }
      })

      output$result_text <- shiny::renderText({
        value <- detail()
        if (is.null(value) || !isTRUE(value$ok)) return("")
        .studio_result_text(value$result)
      })
    }
  )
}

#' @keywords internal
.studio_merge_page <- function(current, page, append = FALSE,
                               requested_cursor = NULL,
                               seen_cursors = character(0)) {
  if (!isTRUE(append) || is.null(current) || !isTRUE(current$ok)) return(page)
  if (!isTRUE(page$ok)) {
    current$page_error <- "The next shared tracking page could not be loaded."
    return(current)
  }
  if (length(intersect(current$jobs$tracking_id, page$jobs$tracking_id))) {
    current$page_error <- "The next shared tracking page could not be loaded."
    return(current)
  }
  prior_cursors <- unique(c(seen_cursors, requested_cursor))
  if (isTRUE(page$has_more) && page$next_cursor %in% prior_cursors) {
    current$page_error <- "The next shared tracking page could not be loaded."
    return(current)
  }
  page$jobs <- rbind(current$jobs, page$jobs)
  rownames(page$jobs) <- NULL
  page$page_error <- NULL
  page
}

#' @keywords internal
.studio_ui <- function(conns) {
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(.studio_css())),
    shiny::div(class = "studio-shell",
      shiny::div(class = "studio-topbar",
        shiny::div(class = "studio-brand",
          shiny::tags$h1("dsHPC Studio"),
          shiny::tags$span("Shared disclosure-safe analyses")),
        shiny::div(class = "studio-actions",
          shiny::selectInput("server", "Server", choices = names(conns),
            width = "220px"),
          shiny::actionButton("refresh", "Refresh", class = "btn-primary"))),
      shiny::uiOutput("status_bar"),
      shiny::div(class = "lookup-row",
        shiny::textInput("lookup_id", "Find by tracking ID", value = "",
          placeholder = "trk_...", width = "420px"),
        shiny::actionButton("lookup", "Find")),
      shiny::div(class = "studio-grid",
        shiny::div(class = "studio-pane",
          shiny::div(class = "pane-head",
            shiny::tags$h2("Shared analyses"),
            shiny::selectInput("state_filter", "State",
              choices = c("All" = "all", "Queued" = "queued",
                "Running" = "running", "Terminal" = "terminal"),
              selected = "all", width = "160px")),
          shiny::uiOutput("job_list"),
          shiny::uiOutput("more_button")),
        shiny::div(class = "studio-pane",
          shiny::uiOutput("job_detail"),
          shiny::tags$h3("Reusable outputs"),
          shiny::tableOutput("outputs_table"),
          shiny::uiOutput("output_loader"),
          shiny::uiOutput("assignment_status"),
          shiny::tags$h3("Disclosure-safe result"),
          shiny::verbatimTextOutput("result_text"))))
  )
}

#' @keywords internal
.studio_filter_jobs_by_state <- function(jobs, state_filter = "all") {
  if (!is.data.frame(jobs) || !nrow(jobs)) return(.studio_empty_jobs())
  if (!identical(state_filter, "all")) {
    jobs <- jobs[jobs$state == state_filter, , drop = FALSE]
  }
  jobs
}

#' @keywords internal
.studio_job_list <- function(jobs, active_id = NULL) {
  if (!is.data.frame(jobs) || !nrow(jobs)) {
    return(shiny::div(class = "empty-state", "No shared analyses"))
  }
  shiny::div(class = "job-list", lapply(seq_len(nrow(jobs)), function(index) {
    .studio_job_card(jobs[index, , drop = FALSE],
      active = identical(jobs$tracking_id[index], active_id))
  }))
}

#' @keywords internal
.studio_job_card <- function(row, active = FALSE) {
  state <- row$state[1L]
  id <- row$tracking_id[1L]
  onclick <- paste0("Shiny.setInputValue('selected_tracking_id', '",
    .studio_html_escape(id), "', {priority: 'event'})")
  shiny::tags$button(type = "button",
    class = paste("job-card", state, if (active) "active" else ""),
    onclick = onclick,
    shiny::div(class = "job-card-main",
      shiny::span(class = paste("state-pill", state), state),
      shiny::tags$strong(.studio_kind_label(row$kind[1L]))),
    shiny::div(class = "job-card-meta", id))
}

#' @keywords internal
.studio_kind_label <- function(kind) {
  switch(as.character(kind),
    imaging = "Shared imaging analysis",
    "Shared analysis")
}

#' @keywords internal
.studio_job_detail <- function(status) {
  shiny::div(class = "detail-wrap",
    shiny::div(class = "detail-title",
      shiny::tags$h2(.studio_kind_label(status$kind)),
      shiny::span(class = paste("state-pill", status$state), status$state)),
    .studio_field("Tracking ID", status$tracking_id),
    .studio_field("Type", status$kind),
    .studio_field("Terminal", if (isTRUE(status$is_done)) "yes" else "no"))
}

#' @keywords internal
.studio_outputs_table <- function(outputs) {
  if (!is.data.frame(outputs) || !nrow(outputs)) return(data.frame())
  outputs[, c("name", "kind", "classification"), drop = FALSE]
}

#' @keywords internal
.studio_result_text <- function(result) {
  if (is.null(result)) return("Result not ready")
  if (!isTRUE(result$ready)) {
    if (identical(result$error, "Job execution failed.")) {
      return("Job execution failed.")
    }
    return("Result not ready")
  }
  paste(utils::capture.output(utils::str(
    result[c("summaries", "available_outputs")], max.level = 4L,
    give.attr = FALSE)), collapse = "\n")
}

#' @keywords internal
.studio_field <- function(label, value) {
  value <- as.character(value %||% "")
  if (!length(value) || is.na(value[1L]) || !nzchar(value[1L])) value <- "-"
  shiny::div(class = "detail-field", shiny::tags$span(label),
    shiny::tags$strong(value[1L]))
}

#' @keywords internal
.studio_html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

#' @keywords internal
.studio_css <- function() {
  "
  body { background:#F8FAFC; color:#111827; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif; }
  .studio-shell { max-width:1180px; margin:0 auto; padding:18px; }
  .studio-topbar { display:flex; justify-content:space-between; gap:18px; align-items:flex-end; border-bottom:1px solid #D1D5DB; padding-bottom:14px; }
  .studio-brand h1 { margin:0; font-size:28px; }
  .studio-brand span, .job-card-meta { color:#64748B; font-size:13px; }
  .studio-actions, .lookup-row { display:flex; gap:10px; align-items:flex-end; flex-wrap:wrap; }
  .studio-actions .form-group, .lookup-row .form-group { margin-bottom:0; }
  .lookup-row { margin:14px 0; }
  .status-line { margin-top:14px; padding:10px 12px; border:1px solid #D1D5DB; border-radius:6px; background:#FFFFFF; }
  .status-line.ok { border-left:4px solid #16A34A; }
  .status-line.error { border-left:4px solid #DC2626; color:#7F1D1D; background:#FEF2F2; }
  .status-line.muted, .empty-state { color:#64748B; }
  .studio-grid { display:grid; grid-template-columns:minmax(330px,.8fr) minmax(480px,1.2fr); gap:14px; align-items:start; }
  .studio-pane { background:#FFFFFF; border:1px solid #D1D5DB; border-radius:6px; padding:14px; min-width:0; }
  .pane-head, .detail-title { display:flex; justify-content:space-between; gap:12px; align-items:flex-end; }
  .pane-head h2, .detail-title h2 { margin:0; font-size:18px; }
  .job-list { display:flex; flex-direction:column; gap:9px; max-height:68vh; overflow:auto; margin-bottom:10px; }
  .job-card { width:100%; text-align:left; background:#FFFFFF; border:1px solid #D1D5DB; border-left:5px solid #64748B; border-radius:6px; padding:11px; cursor:pointer; color:#111827; }
  .job-card:hover, .job-card.active { border-color:#2563EB; box-shadow:0 0 0 2px #DBEAFE; }
  .job-card.running { border-left-color:#2563EB; }
  .job-card.queued { border-left-color:#D97706; }
  .job-card.terminal { border-left-color:#16A34A; }
  .job-card-main { display:flex; align-items:center; gap:8px; }
  .job-card-meta { margin-top:8px; overflow-wrap:anywhere; }
  .state-pill { border-radius:999px; padding:4px 9px; font-size:12px; font-weight:700; background:#E5E7EB; }
  .state-pill.running { background:#DBEAFE; color:#1D4ED8; }
  .state-pill.queued { background:#FEF3C7; color:#92400E; }
  .state-pill.terminal { background:#DCFCE7; color:#166534; }
  .detail-field { border-bottom:1px solid #E5E7EB; padding:9px 0; }
  .detail-field span { display:block; color:#64748B; font-size:12px; }
  .detail-field strong { overflow-wrap:anywhere; }
  .output-loader { display:flex; gap:10px; align-items:flex-end; flex-wrap:wrap; margin:12px 0; }
  .output-loader .form-group { margin-bottom:0; }
  .empty-state { padding:22px; text-align:center; }
  table { font-size:13px; }
  @media (max-width:800px) { .studio-grid { grid-template-columns:1fr; } }
  "
}

# Module: dsHPC Studio

#' Launch dsHPC Studio
#'
#' Opens a local Shiny dashboard for browsing dsHPC jobs on a pool of
#' DataSHIELD connections. Studio queries only the selected server and refreshes
#' on demand.
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

#' Fetch dsHPC Studio data
#'
#' @param conns DSI connections object.
#' @param server Character server name. If NULL, all servers are queried.
#' @param label Character or NULL; optional job label filter.
#' @param mode Character; `"mine"`, `"mine+global"`, or `"global"`.
#' @return A `dshpc_result` with one Studio snapshot per queried server.
#' @export
ds.hpc.studio_data <- function(conns, server = NULL, label = NULL,
                               mode = "mine+global") {
  mode <- match.arg(mode, c("mine", "mine+global", "global"))
  conns <- .studio_named_conns(conns)
  servers <- server %||% names(conns)
  bad <- setdiff(servers, names(conns))
  if (length(bad) > 0)
    stop("Unknown server: ", bad[1], call. = FALSE)

  out <- lapply(servers, function(srv) {
    .studio_fetch_one(conns, srv, label = label, mode = mode)
  })
  names(out) <- servers
  dshpc_result(per_site = out, meta = list(scope = "studio"))
}

#' @keywords internal
.studio_named_conns <- function(conns) {
  n <- names(conns)
  if (is.null(n) || length(n) != length(conns)) {
    n <- rep("", length(conns))
  }
  missing <- is.na(n) | !nzchar(n)
  if (any(missing)) n[missing] <- paste0("server_", which(missing))
  names(conns) <- n
  conns
}

#' @keywords internal
.studio_fetch_one <- function(conns, server, label = NULL,
                              mode = "mine+global") {
  started <- Sys.time()
  tryCatch({
    backend <- .detect_backend(conns[[server]])
    scope <- .ds_encode(list(.owner = backend$username %||% "anonymous"))
    label <- .studio_null_if_empty(label)
    res <- DSI::datashield.aggregate(conns[server],
      expr = call("hpcStudioDS", scope, label, mode))
    snapshot <- res[[server]] %||% res[[1]]
    .studio_normalize_snapshot(snapshot, server = server,
      fetched_at = started, error = NULL)
  }, error = function(e) {
    .studio_error_snapshot(server, conditionMessage(e), started)
  })
}

#' @keywords internal
.studio_cancel_one <- function(conns, server, job_id, admin_key) {
  tryCatch({
    key_enc <- .ds_encode(list(.admin_key = admin_key))
    res <- DSI::datashield.aggregate(conns[server],
      expr = call("hpcAdminCancelDS", job_id, key_enc))
    list(ok = TRUE, server = server, job_id = job_id,
      result = res[[server]] %||% res[[1]], error = NULL,
      timestamp = Sys.time())
  }, error = function(e) {
    list(ok = FALSE, server = server, job_id = job_id, result = NULL,
      error = conditionMessage(e), timestamp = Sys.time())
  })
}

#' @keywords internal
.studio_normalize_snapshot <- function(x, server, fetched_at = Sys.time(),
                                       error = NULL) {
  if (!is.list(x)) x <- list()
  x$ok <- is.null(error)
  x$error <- error
  x$server <- server
  x$fetched_at <- fetched_at
  x$jobs <- .studio_df(x$jobs, .studio_empty_jobs())
  if (nrow(x$jobs) > 0 && "submitted_at" %in% names(x$jobs)) {
    x$jobs <- x$jobs[order(x$jobs$submitted_at, decreasing = TRUE,
      na.last = TRUE), , drop = FALSE]
  }
  x$steps <- .studio_df(x$steps, .studio_empty_steps())
  x$dag_nodes <- .studio_df(x$dag_nodes, .studio_empty_dag_nodes())
  x$dag_edges <- .studio_df(x$dag_edges, .studio_empty_dag_edges())
  x$outputs <- .studio_df(x$outputs, .studio_empty_outputs())
  x$events <- .studio_df(x$events, .studio_empty_events())
  x$scheduler <- x$scheduler %||% list()
  x
}

#' @keywords internal
.studio_error_snapshot <- function(server, error, fetched_at = Sys.time()) {
  .studio_normalize_snapshot(list(), server = server, fetched_at = fetched_at,
    error = error)
}

#' @keywords internal
.studio_empty_jobs <- function() {
  data.frame(
    job_id = character(0), state = character(0), scope = character(0),
    visibility = character(0), name = character(0), label = character(0),
    resource_class = character(0), priority = integer(0),
    submitted_at = character(0), accepted_at = character(0),
    started_at = character(0), finished_at = character(0),
    elapsed_seconds = numeric(0), queue_seconds = numeric(0),
    step_index = integer(0), total_steps = integer(0),
    progress = character(0), progress_percent = numeric(0),
    retry_count = integer(0), error_class = character(0),
    error = character(0), stringsAsFactors = FALSE)
}

#' @keywords internal
.studio_empty_steps <- function() {
  data.frame(job_id = character(0), step_index = integer(0),
    node_id = character(0), type = character(0), plane = character(0),
    runner = character(0), state = character(0),
    started_at = character(0), finished_at = character(0),
    duration_seconds = numeric(0), exit_code = integer(0),
    error_class = character(0), error = character(0),
    external_backend = character(0), external_status = character(0),
    stringsAsFactors = FALSE)
}

#' @keywords internal
.studio_empty_dag_nodes <- function() {
  data.frame(job_id = character(0), step_index = integer(0),
    node_id = character(0), type = character(0), plane = character(0),
    runner = character(0), state = character(0), stringsAsFactors = FALSE)
}

#' @keywords internal
.studio_empty_dag_edges <- function() {
  data.frame(job_id = character(0), from_step = integer(0),
    to_step = integer(0), from_node = character(0),
    to_node = character(0), input_name = character(0),
    stringsAsFactors = FALSE)
}

#' @keywords internal
.studio_empty_outputs <- function() {
  data.frame(job_id = character(0), step_index = integer(0),
    name = character(0), kind = character(0), safe_for_client = logical(0),
    size_bytes = numeric(0), created_at = character(0),
    stringsAsFactors = FALSE)
}

#' @keywords internal
.studio_empty_events <- function() {
  data.frame(job_id = character(0), event = character(0),
    timestamp = character(0), stringsAsFactors = FALSE)
}

#' @keywords internal
.studio_df <- function(x, empty) {
  if (is.data.frame(x)) return(x)
  empty
}

#' @keywords internal
.studio_app <- function(conns) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::tags$head(shiny::tags$style(.studio_css())),
      shiny::div(class = "studio-shell",
        shiny::div(class = "studio-topbar",
          shiny::div(class = "studio-brand",
            shiny::tags$h1("dsHPC Studio"),
            shiny::tags$span("DataSHIELD job monitor")),
          shiny::div(class = "studio-actions",
            shiny::selectInput("server", "Server", choices = names(conns),
              width = "220px"),
            shiny::selectInput("mode", "Scope",
              choices = c("Mine + global" = "mine+global",
                "Mine" = "mine", "Global" = "global"),
              selected = "mine+global", width = "160px"),
            shiny::textInput("label", "Label", value = "", width = "180px"),
            shiny::actionButton("refresh", "Refresh", class = "btn-primary"))),
        shiny::uiOutput("status_bar"),
        shiny::uiOutput("summary_tiles"),
        shiny::div(class = "studio-grid",
          shiny::div(class = "studio-pane jobs-pane",
            shiny::div(class = "pane-head",
              shiny::tags$h2("Jobs"),
              shiny::selectInput("state_filter", "State",
                choices = c("All" = "ALL", "Running" = "RUNNING",
                  "Pending" = "PENDING", "Finished" = "FINISHED",
                  "Published" = "PUBLISHED", "Failed" = "FAILED",
                  "Cancelled" = "CANCELLED"),
                selected = "ALL", width = "180px")),
            shiny::uiOutput("job_list")),
          shiny::div(class = "studio-pane detail-pane",
            shiny::tabsetPanel(id = "tabs",
              shiny::tabPanel("Overview", shiny::uiOutput("job_detail")),
              shiny::tabPanel("DAG", shiny::uiOutput("dag_view")),
              shiny::tabPanel("Steps", shiny::tableOutput("steps_table")),
              shiny::tabPanel("Outputs", shiny::tableOutput("outputs_table")),
              shiny::tabPanel("Events", shiny::tableOutput("events_table")),
              shiny::tabPanel("Scheduler", shiny::uiOutput("scheduler_view"))
            )
          )
        ),
        shiny::uiOutput("cancel_result")
      )
    ),
    server = function(input, output, session) {
      snapshot <- shiny::reactiveVal(NULL)
      selected_job_value <- shiny::reactiveVal(NULL)
      cancel_result <- shiny::reactiveVal(NULL)

      fetch <- function() {
        srv <- input$server %||% names(conns)[1]
        snap <- .studio_fetch_one(conns, srv, label = input$label,
          mode = input$mode %||% "mine+global")
        snapshot(snap)
      }

      shiny::observeEvent(list(input$server, input$mode), fetch(),
        ignoreInit = FALSE)
      shiny::observeEvent(input$refresh, fetch(), ignoreInit = TRUE)

      filtered_jobs <- shiny::reactive({
        snap <- snapshot()
        if (is.null(snap) || !isTRUE(snap$ok)) return(.studio_empty_jobs())
        .studio_filter_jobs_by_state(snap$jobs, input$state_filter %||% "ALL")
      })

      shiny::observe({
        jobs <- filtered_jobs()
        if (nrow(jobs) == 0) {
          selected_job_value(NULL)
          return()
        }
        selected <- shiny::isolate(selected_job_value())
        if (is.null(selected) || !selected %in% jobs$job_id) {
          selected_job_value(jobs$job_id[1])
        }
      })

      shiny::observeEvent(input$selected_job_click, {
        selected_job_value(input$selected_job_click)
      })

      selected_job <- shiny::reactive({
        jobs <- filtered_jobs()
        if (nrow(jobs) == 0) return(NULL)
        job_id <- selected_job_value() %||% jobs$job_id[1]
        row <- jobs[jobs$job_id == job_id, , drop = FALSE]
        if (nrow(row) == 0) jobs[1, , drop = FALSE] else row
      })

      selected_job_id <- shiny::reactive({
        row <- selected_job()
        if (is.null(row) || nrow(row) == 0) NULL else row$job_id[1]
      })

      output$status_bar <- shiny::renderUI({
        snap <- snapshot()
        if (is.null(snap)) {
          return(shiny::div(class = "status-line muted", "Waiting for data"))
        }
        if (!isTRUE(snap$ok)) {
          return(shiny::div(class = "status-line error",
            shiny::tags$strong(snap$server), ": ", snap$error))
        }
        shiny::div(class = "status-line ok",
          shiny::tags$strong(snap$server), " | ",
          "Last refresh: ", format(snap$fetched_at, "%H:%M:%S"), " | ",
          "Server time: ", .studio_display_time(snap$server_time))
      })

      output$summary_tiles <- shiny::renderUI({
        snap <- snapshot()
        jobs <- if (!is.null(snap) && isTRUE(snap$ok)) snap$jobs
          else .studio_empty_jobs()
        states <- .studio_state_counts(jobs)
        shiny::div(class = "summary-row",
          .studio_tile("Total", nrow(jobs), "neutral"),
          .studio_tile("Running", states[["RUNNING"]] %||% 0L, "blue"),
          .studio_tile("Pending", states[["PENDING"]] %||% 0L, "amber"),
          .studio_tile("Finished", (states[["FINISHED"]] %||% 0L) +
              (states[["PUBLISHED"]] %||% 0L), "green"),
          .studio_tile("Failed", (states[["FAILED"]] %||% 0L) +
              (states[["CANCELLED"]] %||% 0L), "red"))
      })

      output$job_list <- shiny::renderUI({
        .studio_job_list(filtered_jobs(), active_id = selected_job_id())
      })

      output$job_detail <- shiny::renderUI({
        snap <- snapshot()
        row <- selected_job()
        if (is.null(snap) || !isTRUE(snap$ok)) {
          return(shiny::div(class = "empty-state", "No data"))
        }
        if (is.null(row)) {
          return(shiny::div(class = "empty-state", "No jobs"))
        }
        .studio_job_detail(row)
      })

      output$dag_view <- shiny::renderUI({
        snap <- snapshot()
        job_id <- selected_job_id()
        if (is.null(snap) || !isTRUE(snap$ok) || is.null(job_id)) {
          return(shiny::div(class = "empty-state", "No DAG"))
        }
        nodes <- snap$dag_nodes[snap$dag_nodes$job_id == job_id, , drop = FALSE]
        edges <- snap$dag_edges[snap$dag_edges$job_id == job_id, , drop = FALSE]
        shiny::div(class = "dag-scroll",
          shiny::HTML(.studio_dag_svg(nodes, edges)))
      })

      output$steps_table <- shiny::renderTable({
        snap <- snapshot()
        job_id <- selected_job_id()
        if (is.null(snap) || is.null(job_id)) return(data.frame())
        .studio_steps_table(snap$steps[snap$steps$job_id == job_id,
          , drop = FALSE])
      }, striped = TRUE, bordered = FALSE, spacing = "s",
      rownames = FALSE)

      output$outputs_table <- shiny::renderTable({
        snap <- snapshot()
        job_id <- selected_job_id()
        if (is.null(snap) || is.null(job_id)) return(data.frame())
        .studio_outputs_table(snap$outputs[snap$outputs$job_id == job_id,
          , drop = FALSE])
      }, striped = TRUE, bordered = FALSE, spacing = "s",
      rownames = FALSE)

      output$events_table <- shiny::renderTable({
        snap <- snapshot()
        job_id <- selected_job_id()
        if (is.null(snap) || is.null(job_id)) return(data.frame())
        snap$events[snap$events$job_id == job_id,
          c("timestamp", "event"), drop = FALSE]
      }, striped = TRUE, bordered = FALSE, spacing = "s",
      rownames = FALSE)

      output$scheduler_view <- shiny::renderUI({
        snap <- snapshot()
        if (is.null(snap) || !isTRUE(snap$ok)) {
          return(shiny::div(class = "empty-state", "No scheduler data"))
        }
        .studio_scheduler_view(snap$scheduler)
      })

      shiny::observeEvent(input$cancel_job, {
        job_id <- selected_job_id()
        if (is.null(job_id)) return()
        shiny::showModal(shiny::modalDialog(
          title = "Cancel job",
          shiny::tags$p(shiny::tags$strong(
            paste0("SEGURO que quieres cancelar el job ", job_id, "?"))),
          shiny::tags$p(
            "This sends an admin cancellation request to the selected server."),
          shiny::passwordInput("admin_password", "Admin password", value = ""),
          footer = shiny::tagList(
            shiny::modalButton("Back"),
            shiny::actionButton("confirm_cancel", "Cancel job",
              class = "btn-danger")
          ),
          easyClose = TRUE
        ))
      })

      shiny::observeEvent(input$confirm_cancel, {
        job_id <- selected_job_id()
        key <- input$admin_password
        if (is.null(job_id) || is.null(key) || !nzchar(key)) return()
        res <- .studio_cancel_one(conns, input$server, job_id, key)
        shiny::updateTextInput(session, "admin_password", value = "")
        shiny::removeModal()
        cancel_result(res)
        fetch()
      })

      output$cancel_result <- shiny::renderUI({
        res <- cancel_result()
        if (is.null(res)) return(NULL)
        cls <- if (isTRUE(res$ok)) "status-line ok" else "status-line error"
        msg <- if (isTRUE(res$ok)) {
          paste0("Cancellation requested for ", res$job_id, " on ", res$server)
        } else {
          paste0("Cancellation failed for ", res$job_id, ": ", res$error)
        }
        shiny::div(class = cls, msg)
      })
    }
  )
}

#' @keywords internal
.studio_null_if_empty <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  x <- as.character(x[1])
  if (is.na(x) || !nzchar(trimws(x))) NULL else x
}

#' @keywords internal
.studio_state_counts <- function(jobs) {
  if (!is.data.frame(jobs) || nrow(jobs) == 0) return(list())
  tab <- table(as.character(jobs$state))
  as.list(stats::setNames(as.integer(tab), names(tab)))
}

#' @keywords internal
.studio_tile <- function(label, value, tone = "neutral") {
  shiny::div(class = paste("summary-tile", tone),
    shiny::tags$span(class = "tile-label", label),
    shiny::tags$strong(value))
}

#' @keywords internal
.studio_filter_jobs_by_state <- function(jobs, state_filter = "ALL") {
  if (!is.data.frame(jobs) || nrow(jobs) == 0) return(.studio_empty_jobs())
  state_filter <- state_filter %||% "ALL"
  if (!identical(state_filter, "ALL")) {
    jobs <- jobs[toupper(jobs$state) == state_filter, , drop = FALSE]
  }
  jobs
}

#' @keywords internal
.studio_job_list <- function(jobs, active_id = NULL) {
  if (!is.data.frame(jobs) || nrow(jobs) == 0) {
    return(shiny::div(class = "empty-state", "No jobs"))
  }
  shiny::div(class = "job-list",
    lapply(seq_len(nrow(jobs)), function(i) {
      .studio_job_card(jobs[i, , drop = FALSE],
        active = identical(jobs$job_id[i], active_id))
    }))
}

#' @keywords internal
.studio_job_card <- function(row, active = FALSE) {
  state <- toupper(row$state[1])
  tone <- .studio_state_tone(state)
  job_id <- row$job_id[1]
  progress <- suppressWarnings(as.numeric(row$progress_percent[1]))
  if (!is.finite(progress)) progress <- 0
  onclick <- paste0("Shiny.setInputValue('selected_job_click', '",
    .studio_html_escape(job_id), "', {priority: 'event'})")
  shiny::tags$button(type = "button",
    class = paste("job-card", tone, if (active) "active" else ""),
    onclick = onclick,
    shiny::div(class = "job-card-main",
      shiny::span(class = paste("state-pill", tolower(state)), state),
      shiny::tags$strong(row$name[1])),
    shiny::div(class = "job-card-meta",
      shiny::span(row$job_id[1]),
      shiny::span(row$scope[1]),
      shiny::span(row$label[1])),
    shiny::div(class = "job-card-metrics",
      shiny::span(paste("Progress", row$progress[1])),
      shiny::span(paste("Elapsed",
        .studio_format_duration(row$elapsed_seconds[1]))),
      shiny::span(.studio_short_time(row$submitted_at[1]))),
    shiny::div(class = "job-progress-track",
      shiny::div(class = "job-progress-fill",
        style = paste0("width:", max(0, min(100, progress)), "%"))))
}

#' @keywords internal
.studio_state_tone <- function(state) {
  switch(toupper(as.character(state %||% "")[1]),
    RUNNING = "running", PENDING = "pending", FINISHED = "finished",
    PUBLISHED = "published", FAILED = "failed", CANCELLED = "cancelled",
    DONE = "finished", "neutral")
}

#' @keywords internal
.studio_state_terminal <- function(state) {
  toupper(as.character(state %||% "")[1]) %in%
    c("FINISHED", "PUBLISHED", "FAILED", "CANCELLED")
}

#' @keywords internal
.studio_jobs_table <- function(jobs) {
  if (!is.data.frame(jobs) || nrow(jobs) == 0) return(data.frame())
  data.frame(
    state = jobs$state,
    name = jobs$name,
    job_id = jobs$job_id,
    scope = jobs$scope,
    label = jobs$label,
    progress = jobs$progress,
    elapsed = .studio_format_duration(jobs$elapsed_seconds),
    submitted = .studio_short_time(jobs$submitted_at),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.studio_steps_table <- function(steps) {
  if (!is.data.frame(steps) || nrow(steps) == 0) return(data.frame())
  data.frame(
    step = steps$step_index,
    node = steps$node_id,
    state = steps$state,
    type = steps$type,
    plane = steps$plane,
    runner = steps$runner,
    backend = steps$external_backend,
    external = steps$external_status,
    duration = .studio_format_duration(steps$duration_seconds),
    error = steps$error,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.studio_outputs_table <- function(outputs) {
  if (!is.data.frame(outputs) || nrow(outputs) == 0) return(data.frame())
  data.frame(
    step = outputs$step_index,
    name = outputs$name,
    kind = outputs$kind,
    safe = outputs$safe_for_client,
    size = .studio_format_bytes(outputs$size_bytes),
    created = .studio_short_time(outputs$created_at),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.studio_job_detail <- function(row) {
  status_class <- paste0("state-pill ", tolower(row$state[1]))
  shiny::div(class = "detail-wrap",
    shiny::div(class = "detail-title",
      shiny::div(
        shiny::tags$h2(row$name[1]),
        shiny::tags$span(class = "detail-subtitle", row$job_id[1])),
      shiny::div(class = "detail-actions",
        shiny::span(class = status_class, row$state[1]),
        if (!.studio_state_terminal(row$state[1])) {
          shiny::actionButton("cancel_job", "Cancel job",
            class = "btn-danger btn-sm")
        })),
    shiny::div(class = "progress-track",
      shiny::div(class = "progress-fill",
        style = paste0("width:", row$progress_percent[1], "%"))),
    shiny::div(class = "detail-grid",
      .studio_field("Scope", row$scope[1]),
      .studio_field("Visibility", row$visibility[1]),
      .studio_field("Name", row$name[1]),
      .studio_field("Label", row$label[1]),
      .studio_field("Resource", row$resource_class[1]),
      .studio_field("Progress", row$progress[1]),
      .studio_field("Retries", row$retry_count[1]),
      .studio_field("Queue", .studio_format_duration(row$queue_seconds[1])),
      .studio_field("Elapsed", .studio_format_duration(row$elapsed_seconds[1])),
      .studio_field("Submitted", .studio_display_time(row$submitted_at[1])),
      .studio_field("Started", .studio_display_time(row$started_at[1])),
      .studio_field("Finished", .studio_display_time(row$finished_at[1])),
      .studio_field("Error", row$error[1])
    )
  )
}

#' @keywords internal
.studio_field <- function(label, value) {
  value <- as.character(value %||% "")
  if (length(value) == 0 || is.na(value[1]) || !nzchar(value[1])) value <- "-"
  shiny::div(class = "detail-field",
    shiny::tags$span(label),
    shiny::tags$strong(value[1]))
}

#' @keywords internal
.studio_scheduler_view <- function(scheduler) {
  executor <- scheduler$executor %||% list()
  cell <- scheduler$cell %||% list()
  node <- scheduler$node %||% list()
  usage <- scheduler$usage %||% list()

  shiny::div(class = "scheduler-grid",
    shiny::div(class = "scheduler-block",
      shiny::tags$h3("Executor"),
      .studio_field("Backend", executor$backend),
      .studio_field("Available", executor$available),
      .studio_field("Reason", executor$reason),
      .studio_field("Delegates resources", executor$delegates_resources)),
    shiny::div(class = "scheduler-block",
      shiny::tags$h3("Cell"),
      .studio_field("Cell", cell$cell_id),
      .studio_field("Node", cell$node_id),
      .studio_field("Leader active", cell$leader_active),
      .studio_field("Workers", cell$worker_count)),
    shiny::div(class = "scheduler-block",
      shiny::tags$h3("Node"),
      .studio_field("Memory MB", node$memory_mb),
      .studio_field("CPU slots", node$cpu_slots),
      .studio_field("GPUs", node$gpus),
      .studio_field("GPU backend", node$gpu_backend)),
    shiny::div(class = "scheduler-block",
      shiny::tags$h3("Usage"),
      .studio_field("Running jobs", usage$running_jobs),
      .studio_field("Memory MB", usage$memory_mb),
      .studio_field("CPU slots", usage$cpu_slots),
      .studio_field("GPUs", usage$gpus))
  )
}

#' @keywords internal
.studio_dag_svg <- function(nodes, edges) {
  if (!is.data.frame(nodes) || nrow(nodes) == 0) {
    return("<div class=\"empty-state\">No DAG</div>")
  }
  layout <- .studio_dag_layout(nodes, edges)
  width <- max(760, max(layout$x) + 230)
  height <- max(240, max(layout$y) + 150)
  node_svg <- vapply(seq_len(nrow(layout)), function(i) {
    row <- layout[i, ]
    fill <- .studio_state_fill(row$state)
    stroke <- .studio_state_stroke(row$state)
    runner <- if (nzchar(row$runner)) paste0("runner: ", row$runner)
      else paste0("plane: ", row$plane)
    paste0(
      "<g>",
      "<rect x=\"", row$x, "\" y=\"", row$y,
      "\" width=\"170\" height=\"86\" rx=\"8\" fill=\"", fill,
      "\" stroke=\"", stroke, "\" stroke-width=\"2\"/>",
      "<text x=\"", row$x + 12, "\" y=\"", row$y + 24,
      "\" class=\"dag-title\">", .studio_html_escape(row$node_id), "</text>",
      "<text x=\"", row$x + 12, "\" y=\"", row$y + 46,
      "\" class=\"dag-text\">", .studio_html_escape(row$type), "</text>",
      "<text x=\"", row$x + 12, "\" y=\"", row$y + 64,
      "\" class=\"dag-text muted-svg\">", .studio_html_escape(runner), "</text>",
      "<text x=\"", row$x + 12, "\" y=\"", row$y + 80,
      "\" class=\"dag-text muted-svg\">", .studio_html_escape(row$state), "</text>",
      "</g>")
  }, character(1))

  edge_svg <- character(0)
  if (is.data.frame(edges) && nrow(edges) > 0) {
    by_step <- stats::setNames(seq_len(nrow(layout)), layout$step_index)
    edge_svg <- vapply(seq_len(nrow(edges)), function(i) {
      edge <- edges[i, ]
      from_i <- by_step[[as.character(edge$from_step)]]
      to_i <- by_step[[as.character(edge$to_step)]]
      if (is.null(from_i) || is.null(to_i)) return("")
      from <- layout[from_i, ]
      to <- layout[to_i, ]
      x1 <- from$x + 170
      y1 <- from$y + 43
      x2 <- to$x
      y2 <- to$y + 43
      mid <- x1 + max(36, (x2 - x1) / 2)
      label <- edge$input_name %||% ""
      label_svg <- if (nzchar(label)) paste0(
        "<text x=\"", (x1 + x2) / 2, "\" y=\"", (y1 + y2) / 2 - 8,
        "\" class=\"edge-label\">", .studio_html_escape(label), "</text>")
      else ""
      paste0("<path d=\"M", x1, " ", y1, " C", mid, " ", y1, " ",
        mid, " ", y2, " ", x2, " ", y2,
        "\" class=\"dag-edge\" marker-end=\"url(#arrow)\"/>", label_svg)
    }, character(1))
  }

  paste0(
    "<svg class=\"dag-svg\" viewBox=\"0 0 ", width, " ", height,
    "\" role=\"img\" aria-label=\"Job DAG\">",
    "<defs><marker id=\"arrow\" markerWidth=\"10\" markerHeight=\"10\" ",
    "refX=\"9\" refY=\"3\" orient=\"auto\" markerUnits=\"strokeWidth\">",
    "<path d=\"M0,0 L0,6 L9,3 z\" fill=\"#475569\"/></marker></defs>",
    paste(edge_svg, collapse = ""),
    paste(node_svg, collapse = ""),
    "</svg>"
  )
}

#' @keywords internal
.studio_dag_layout <- function(nodes, edges) {
  nodes <- nodes[order(nodes$step_index), , drop = FALSE]
  depth <- stats::setNames(rep(1L, nrow(nodes)), as.character(nodes$step_index))
  if (is.data.frame(edges) && nrow(edges) > 0) {
    edges <- edges[order(edges$to_step), , drop = FALSE]
    for (i in seq_len(nrow(edges))) {
      from <- as.character(edges$from_step[i])
      to <- as.character(edges$to_step[i])
      if (from %in% names(depth) && to %in% names(depth)) {
        depth[[to]] <- max(depth[[to]], depth[[from]] + 1L)
      }
    }
  } else if (nrow(nodes) > 1) {
    depth <- stats::setNames(seq_len(nrow(nodes)), as.character(nodes$step_index))
  }
  nodes$depth <- as.integer(depth[as.character(nodes$step_index)])
  nodes$rank <- stats::ave(nodes$step_index, nodes$depth, FUN = seq_along)
  nodes$x <- 40 + (nodes$depth - 1L) * 230
  nodes$y <- 40 + (nodes$rank - 1L) * 135
  nodes
}

#' @keywords internal
.studio_state_fill <- function(state) {
  state <- toupper(as.character(state %||% "")[1])
  switch(state,
    RUNNING = "#DBEAFE",
    PENDING = "#FEF3C7",
    FINISHED = "#DCFCE7",
    PUBLISHED = "#CCFBF1",
    FAILED = "#FEE2E2",
    CANCELLED = "#E5E7EB",
    DONE = "#DCFCE7",
    "#F8FAFC")
}

#' @keywords internal
.studio_state_stroke <- function(state) {
  state <- toupper(as.character(state %||% "")[1])
  switch(state,
    RUNNING = "#2563EB",
    PENDING = "#D97706",
    FINISHED = "#16A34A",
    PUBLISHED = "#0F766E",
    FAILED = "#DC2626",
    CANCELLED = "#4B5563",
    DONE = "#16A34A",
    "#64748B")
}

#' @keywords internal
.studio_html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' @keywords internal
.studio_display_time <- function(x) {
  x <- as.character(x %||% "")
  if (length(x) == 0 || is.na(x[1]) || !nzchar(x[1])) return("unknown")
  value <- x[1]
  parsed <- suppressWarnings(as.POSIXct(value, tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%OS"))
  if (is.na(parsed)) {
    parsed <- suppressWarnings(as.POSIXct(sub("Z$", "", value), tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%OS"))
  }
  if (is.na(parsed)) return(value)
  format(parsed, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
}

#' @keywords internal
.studio_short_time <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  sub("^\\d{4}-", "", sub("Z$", "", x))
}

#' @keywords internal
.studio_format_duration <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep("-", length(x))
  ok <- is.finite(x)
  secs <- floor(x[ok])
  out[ok] <- ifelse(secs < 60, paste0(secs, "s"),
    ifelse(secs < 3600, paste0(floor(secs / 60), "m ", secs %% 60, "s"),
      paste0(floor(secs / 3600), "h ", floor((secs %% 3600) / 60), "m")))
  out
}

#' @keywords internal
.studio_format_bytes <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep("-", length(x))
  ok <- is.finite(x)
  vals <- x[ok]
  out[ok] <- ifelse(vals < 1024, paste0(vals, " B"),
    ifelse(vals < 1024^2, paste0(round(vals / 1024, 1), " KB"),
      ifelse(vals < 1024^3, paste0(round(vals / 1024^2, 1), " MB"),
        paste0(round(vals / 1024^3, 1), " GB"))))
  out
}

#' @keywords internal
.studio_css <- function() {
  "
  body { background:#F8FAFC; color:#111827; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif; }
  .studio-shell { max-width:1440px; margin:0 auto; padding:18px; }
  .studio-topbar { display:flex; justify-content:space-between; gap:18px; align-items:flex-end; border-bottom:1px solid #D1D5DB; padding-bottom:14px; }
  .studio-brand h1 { margin:0; font-size:28px; font-weight:700; letter-spacing:0; }
  .studio-brand span { color:#64748B; font-size:14px; }
  .studio-actions { display:flex; gap:10px; align-items:flex-end; flex-wrap:wrap; }
  .studio-actions .form-group { margin-bottom:0; }
  .btn { border-radius:6px; }
  .btn-primary { background:#2563EB; border-color:#2563EB; }
  .btn-danger { background:#DC2626; border-color:#DC2626; }
  .status-line { margin-top:14px; padding:10px 12px; border-radius:6px; border:1px solid #D1D5DB; background:#FFFFFF; }
  .status-line.ok { border-left:4px solid #16A34A; }
  .status-line.error { border-left:4px solid #DC2626; color:#7F1D1D; background:#FEF2F2; }
  .status-line.muted { color:#64748B; }
  .summary-row { display:grid; grid-template-columns:repeat(5,minmax(120px,1fr)); gap:10px; margin:14px 0; }
  .summary-tile { background:#FFFFFF; border:1px solid #D1D5DB; border-radius:6px; padding:12px; min-height:74px; }
  .summary-tile strong { display:block; font-size:28px; line-height:1.1; margin-top:6px; }
  .tile-label { color:#64748B; font-size:13px; }
  .summary-tile.blue { border-top:3px solid #2563EB; }
  .summary-tile.amber { border-top:3px solid #D97706; }
  .summary-tile.green { border-top:3px solid #16A34A; }
  .summary-tile.red { border-top:3px solid #DC2626; }
  .summary-tile.neutral { border-top:3px solid #64748B; }
  .studio-grid { display:grid; grid-template-columns:minmax(340px,0.8fr) minmax(560px,1.5fr); gap:14px; align-items:start; }
  .studio-pane { background:#FFFFFF; border:1px solid #D1D5DB; border-radius:6px; padding:14px; min-width:0; }
  .pane-head { display:flex; justify-content:space-between; gap:12px; align-items:flex-end; margin-bottom:10px; }
  .pane-head h2, .detail-title h2 { margin:0; font-size:18px; letter-spacing:0; }
  .pane-head .form-group { margin-bottom:0; min-width:160px; }
  table { font-size:13px; }
  .job-list { display:flex; flex-direction:column; gap:10px; max-height:72vh; overflow:auto; padding-right:2px; }
  .job-card { appearance:none; width:100%; text-align:left; background:#FFFFFF; border:1px solid #D1D5DB; border-left:5px solid #64748B; border-radius:6px; padding:11px; cursor:pointer; color:#111827; }
  .job-card:hover, .job-card.active { border-color:#2563EB; box-shadow:0 0 0 2px #DBEAFE; }
  .job-card.running { border-left-color:#2563EB; }
  .job-card.pending { border-left-color:#D97706; }
  .job-card.finished, .job-card.published { border-left-color:#16A34A; }
  .job-card.failed { border-left-color:#DC2626; }
  .job-card.cancelled { border-left-color:#4B5563; }
  .job-card-main { display:flex; align-items:center; gap:8px; min-width:0; }
  .job-card-main strong { font-size:14px; overflow-wrap:anywhere; }
  .job-card-meta, .job-card-metrics { display:flex; flex-wrap:wrap; gap:8px; color:#64748B; font-size:12px; margin-top:8px; }
  .job-progress-track { height:6px; background:#E5E7EB; border-radius:3px; overflow:hidden; margin-top:10px; }
  .job-progress-fill { height:6px; background:#64748B; }
  .job-card.running .job-progress-fill { background:#2563EB; }
  .job-card.pending .job-progress-fill { background:#D97706; }
  .job-card.finished .job-progress-fill, .job-card.published .job-progress-fill { background:#16A34A; }
  .job-card.failed .job-progress-fill { background:#DC2626; }
  .job-card.cancelled .job-progress-fill { background:#4B5563; }
  .detail-title { display:flex; justify-content:space-between; gap:12px; align-items:center; margin-bottom:12px; }
  .detail-subtitle { display:block; color:#64748B; font-size:12px; margin-top:4px; overflow-wrap:anywhere; }
  .detail-actions { display:flex; gap:8px; align-items:center; flex-wrap:wrap; justify-content:flex-end; }
  .state-pill { border-radius:999px; padding:4px 9px; font-size:12px; font-weight:700; background:#E5E7EB; color:#111827; }
  .state-pill.running { background:#DBEAFE; color:#1D4ED8; }
  .state-pill.pending { background:#FEF3C7; color:#92400E; }
  .state-pill.finished, .state-pill.published { background:#DCFCE7; color:#166534; }
  .state-pill.failed { background:#FEE2E2; color:#991B1B; }
  .state-pill.cancelled { background:#E5E7EB; color:#374151; }
  .progress-track { height:10px; background:#E5E7EB; border-radius:5px; overflow:hidden; margin-bottom:14px; }
  .progress-fill { height:10px; background:#2563EB; }
  .detail-grid, .scheduler-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:10px; }
  .detail-field { border-bottom:1px solid #E5E7EB; padding:8px 0; min-width:0; }
  .detail-field span { display:block; color:#64748B; font-size:12px; }
  .detail-field strong { display:block; font-size:13px; overflow-wrap:anywhere; }
  .scheduler-block { border:1px solid #E5E7EB; border-radius:6px; padding:10px; }
  .scheduler-block h3 { margin:0 0 8px 0; font-size:15px; }
  .dag-scroll { overflow:auto; border:1px solid #E5E7EB; border-radius:6px; background:#FFFFFF; }
  .dag-svg { min-width:760px; width:100%; height:auto; display:block; }
  .dag-edge { fill:none; stroke:#475569; stroke-width:2; }
  .dag-title { font-size:14px; font-weight:700; fill:#111827; }
  .dag-text, .edge-label { font-size:12px; fill:#334155; }
  .muted-svg { fill:#64748B; }
  .edge-label { text-anchor:middle; paint-order:stroke; stroke:#FFFFFF; stroke-width:4; }
  .empty-state { color:#64748B; padding:22px; text-align:center; }
  @media (max-width: 900px) {
    .studio-topbar { align-items:stretch; }
    .studio-grid { grid-template-columns:1fr; }
    .summary-row { grid-template-columns:repeat(2,minmax(120px,1fr)); }
    .detail-grid, .scheduler-grid { grid-template-columns:1fr; }
  }
  "
}

.studio_id <- "trk_11111111-1111-4111-8111-111111111111"

.studio_mock_aggregate <- function(conns, expr) {
  method <- as.character(expr[[1L]])
  value <- switch(method,
    hpcCapabilitiesDS = list(shared_tracking = "root_v1",
      shared_results = "safe_v1", reusable_outputs = "opaque_ref_v1",
      queue_visibility = "shared", private_topology = "host-secret"),
    hpcTrackingListDS = list(items = data.frame(
      tracking_id = .studio_id, state = "terminal", is_done = TRUE,
      kind = "analysis", label = "secret cohort", stringsAsFactors = FALSE),
      next_cursor = NULL, has_more = FALSE, schema = "root_v1"),
    hpcTrackingStatusDS = list(tracking_id = .studio_id,
      state = "terminal", is_done = TRUE, kind = "analysis",
      raw_progress = "37/41"),
    hpcTrackingOutputsDS = data.frame(name = "output_001",
      kind = "server_object", classification = "server_reusable",
      path = "/srv/private", stringsAsFactors = FALSE),
    hpcTrackingResultDS = list(ready = TRUE, summaries = list(),
      available_outputs = list(),
      child_job_id = "job_private"),
    stop("unexpected method", call. = FALSE))
  stats::setNames(list(value), names(conns))
}

test_that("Studio data negotiates root_v1 and requests only tracking pages", {
  methods <- character(0)
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      methods <<- c(methods, as.character(expr[[1L]]))
      .studio_mock_aggregate(conns, expr)
    },
    .package = "DSI")

  snapshot <- ds.hpc.studio_data(list(node1 = list()))$per_site$node1
  expect_true(snapshot$ok)
  expect_identical(methods, c("hpcCapabilitiesDS", "hpcTrackingListDS"))
  expect_identical(names(snapshot$jobs),
    c("tracking_id", "state", "is_done", "kind"))
  expect_false(any(c("label", "submitted_at", "progress", "steps", "events",
    "scheduler") %in% names(snapshot)))
})

test_that("Studio keeps loaded roots when the next page fails", {
  first <- list(ok = TRUE, error = NULL, server = "node1",
    jobs = data.frame(tracking_id = .studio_id, state = "running",
      is_done = FALSE, kind = "analysis", stringsAsFactors = FALSE),
    next_cursor = "cur_11111111-1111-4111-8111-111111111111",
    has_more = TRUE, schema = "root_v1", queue_visibility = "shared")
  failed <- dsHPCClient:::.studio_error_snapshot("node1", "remote secret")

  merged <- dsHPCClient:::.studio_merge_page(first, failed, append = TRUE)
  expect_true(merged$ok)
  expect_identical(merged$jobs, first$jobs)
  expect_identical(merged$next_cursor, first$next_cursor)
  expect_false(grepl("remote secret", merged$page_error, fixed = TRUE))
})

test_that("Studio rejects replayed and cyclic continuation cursors", {
  cursor_a <- "cur_11111111-1111-4111-8111-111111111111"
  cursor_b <- "cur_22222222-2222-4222-8222-222222222222"
  first <- list(ok = TRUE, error = NULL, server = "node1",
    jobs = data.frame(tracking_id = .studio_id, state = "running",
      is_done = FALSE, kind = "analysis", stringsAsFactors = FALSE),
    next_cursor = cursor_a, has_more = TRUE, schema = "root_v1",
    queue_visibility = "shared")
  next_page <- first
  next_page$jobs$tracking_id <-
    "trk_22222222-2222-4222-8222-222222222222"

  replay <- dsHPCClient:::.studio_merge_page(first, next_page,
    append = TRUE, requested_cursor = cursor_a)
  expect_identical(replay$jobs, first$jobs)
  expect_match(replay$page_error, "could not be loaded", fixed = TRUE)

  next_page$next_cursor <- cursor_a
  cycle <- dsHPCClient:::.studio_merge_page(first, next_page,
    append = TRUE, requested_cursor = cursor_b, seen_cursors = cursor_a)
  expect_identical(cycle$jobs, first$jobs)
  expect_match(cycle$page_error, "could not be loaded", fixed = TRUE)
})

test_that("manual Studio pages reject a replayed requested cursor", {
  cursor <- "cur_11111111-1111-4111-8111-111111111111"
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      method <- as.character(expr[[1L]])
      value <- if (identical(method, "hpcCapabilitiesDS")) {
        list(shared_tracking = "root_v1", shared_results = "safe_v1",
          reusable_outputs = "opaque_ref_v1", queue_visibility = "shared")
      } else {
        list(items = data.frame(tracking_id = .studio_id,
          state = "running", is_done = FALSE, kind = "analysis",
          stringsAsFactors = FALSE), next_cursor = cursor,
          has_more = TRUE, schema = "root_v1")
      }
      stats::setNames(list(value), names(conns))
    },
    .package = "DSI")

  snapshot <- ds.hpc.studio_data(list(node1 = list()),
    server = "node1", cursor = cursor)$per_site$node1
  expect_false(snapshot$ok)
  expect_identical(snapshot$error,
    "Shared dsHPC tracking is unavailable.")
})

test_that("Studio capability negotiation requires exact field names", {
  partial <- list(shared_tracking_private = "root_v1",
    shared_results_private = "safe_v1",
    reusable_outputs_private = "opaque_ref_v1",
    queue_visibility_private = "shared")
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      stats::setNames(list(partial), names(conns))
    },
    .package = "DSI")

  expect_null(dsHPCClient:::.studio_capabilities(
    list(node1 = list()), "node1"))

  partial <- list(shared_tracking = "root_v1",
    shared_results = "safe_v1", reusable_outputs = "opaque_ref_v1",
    queue_visibility = factor("shared"))
  expect_null(dsHPCClient:::.studio_capabilities(
    list(node1 = list()), "node1"))
})

test_that("Studio never associates another node's response", {
  capabilities <- list(shared_tracking = "root_v1",
    shared_results = "safe_v1", reusable_outputs = "opaque_ref_v1",
    queue_visibility = "shared")
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      stats::setNames(list(capabilities), "other_node")
    },
    .package = "DSI")

  expect_error(dsHPCClient:::.studio_capabilities(
    list(node1 = list()), "node1"), "Invalid Studio response", fixed = TRUE)
  snapshot <- dsHPCClient:::.studio_fetch_one(
    list(node1 = list()), "node1")
  expect_false(snapshot$ok)
  expect_identical(snapshot$error,
    "Shared dsHPC tracking is unavailable.")
})

test_that("Studio rejects a page larger than it requested", {
  rows <- do.call(rbind, lapply(1:3, function(index) {
    data.frame(tracking_id = sub("11111111", sprintf("%08d", index),
      .studio_id, fixed = TRUE), state = "running", is_done = FALSE,
      kind = "analysis", stringsAsFactors = FALSE)
  }))
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      method <- as.character(expr[[1L]])
      value <- if (identical(method, "hpcCapabilitiesDS")) {
        list(shared_tracking = "root_v1", shared_results = "safe_v1",
          reusable_outputs = "opaque_ref_v1", queue_visibility = "shared")
      } else list(items = rows, next_cursor = NULL, has_more = FALSE,
        schema = "root_v1")
      stats::setNames(list(value), names(conns))
    },
    .package = "DSI")

  snapshot <- dsHPCClient:::.studio_fetch_one(
    list(node1 = list()), "node1", limit = 2L)
  expect_false(snapshot$ok)
  expect_equal(nrow(snapshot$jobs), 0L)
})

test_that("Studio degrades cleanly when shared tracking is unavailable", {
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      stats::setNames(list(list(status = "available",
        queue_visibility = "scoped")), names(conns))
    },
    .package = "DSI")

  snapshot <- ds.hpc.studio_data(list(node1 = list()))$per_site$node1
  expect_false(snapshot$ok)
  expect_match(snapshot$error, "unavailable", fixed = TRUE)
  expect_equal(nrow(snapshot$jobs), 0L)
})

test_that("Studio detail strips execution and storage fields", {
  testthat::local_mocked_bindings(
    datashield.aggregate = .studio_mock_aggregate,
    .package = "DSI")

  detail <- dsHPCClient:::.studio_fetch_detail(
    list(node1 = list()), "node1", .studio_id)
  expect_true(detail$ok)
  expect_identical(names(detail$status),
    c("tracking_id", "state", "is_done", "kind"))
  expect_identical(names(detail$outputs),
    c("name", "kind", "classification"))
  expect_false("child_job_id" %in% names(detail$result))
})

test_that("Studio detail must match the requested tracking root", {
  returned <- "trk_22222222-2222-4222-8222-222222222222"
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      method <- as.character(expr[[1L]])
      value <- switch(method,
        hpcTrackingStatusDS = list(tracking_id = returned,
          state = "running", is_done = FALSE, kind = "analysis"),
        stop("unexpected method", call. = FALSE))
      stats::setNames(list(value), names(conns))
    },
    .package = "DSI")

  detail <- dsHPCClient:::.studio_fetch_detail(
    list(node1 = list()), "node1", .studio_id)
  expect_false(detail$ok)
  expect_identical(detail$error, "Shared job details are unavailable.")
})

test_that("Studio rendering contains roots and no operational panels", {
  skip_if_not_installed("shiny")
  jobs <- data.frame(tracking_id = .studio_id, state = "running",
    is_done = FALSE, kind = "analysis", stringsAsFactors = FALSE)
  card <- as.character(dsHPCClient:::.studio_job_list(jobs))
  expect_match(card, "Shared analysis", fixed = TRUE)
  expect_match(card, .studio_id, fixed = TRUE)

  ui <- paste(as.character(dsHPCClient:::.studio_ui(list(node1 = list()))),
    collapse = "\n")
  expect_match(ui, "Find by tracking ID", fixed = TRUE)
  expect_match(ui, "Reusable outputs", fixed = TRUE)
  expect_match(ui, "output_loader", fixed = TRUE)
  expect_false(grepl("Scheduler|DAG|Events|Cancel job|admin_password", ui))
  expect_false(grepl("hpcStudioDS|hpcListDS|hpcSchedulerStatusDS", ui))

  jobs$kind <- "imaging"
  expect_match(as.character(dsHPCClient:::.studio_job_list(jobs)),
    "Shared imaging analysis", fixed = TRUE)
})

test_that("Studio assigns a selected output only through the safe assign API", {
  observed <- NULL
  testthat::local_mocked_bindings(
    ds.hpc.load_output = function(conns, tracking_id, output_name, symbol) {
      observed <<- list(server = names(conns), tracking_id = tracking_id,
        output_name = output_name, symbol = symbol)
      invisible(TRUE)
    },
    .package = "dsHPCClient")

  result <- dsHPCClient:::.studio_assign_one(list(node1 = list()), "node1",
    .studio_id, "output_001", "shared_features")
  expect_true(result$ok)
  expect_identical(observed, list(server = "node1", tracking_id = .studio_id,
    output_name = "output_001", symbol = "shared_features"))
})

test_that("Studio output assignment reflects no remote error detail", {
  secret <- "/srv/private/patient.rds"
  testthat::local_mocked_bindings(
    ds.hpc.load_output = function(...) stop(secret, call. = FALSE),
    .package = "dsHPCClient")
  result <- dsHPCClient:::.studio_assign_one(list(node1 = list()), "node1",
    .studio_id, "output_001", "shared_features")
  expect_false(result$ok)
  expect_false(grepl(secret, result$error, fixed = TRUE))
})

test_that("Studio output table exposes only reusable metadata", {
  outputs <- data.frame(name = "output_001", kind = "server_object",
    classification = "server_reusable", stringsAsFactors = FALSE)
  rendered <- dsHPCClient:::.studio_outputs_table(outputs)
  expect_identical(names(rendered), c("name", "kind", "classification"))
})

test_that("Studio renders the generic terminal failure", {
  expect_identical(dsHPCClient:::.studio_result_text(list(
    state = "terminal", ready = FALSE, error = "Job execution failed.")),
  "Job execution failed.")
  expect_identical(dsHPCClient:::.studio_result_text(list(
    state = "running", ready = FALSE, error = NA_character_)),
  "Result not ready")
})

test_that("Studio loads beyond 100 roots and directly finds root 125", {
  skip_if_not_installed("shiny")
  roots <- data.frame(
    tracking_id = vapply(seq_len(125L), function(index) {
      sprintf("trk_%08d-1111-4111-8111-111111111111", index)
    }, character(1)),
    state = "running", is_done = FALSE, kind = "analysis",
    stringsAsFactors = FALSE)
  target_id <- roots$tracking_id[[125L]]
  detail_ids <- character(0)

  testthat::local_mocked_bindings(
    .studio_fetch_one = function(conns, server, limit = 100L,
                                 cursor = NULL) {
      expect_identical(names(conns), "node1")
      expect_identical(server, "node1")
      expect_identical(limit, 100L)
      if (is.null(cursor)) {
        items <- roots[seq_len(100L), , drop = FALSE]
        next_cursor <- "cursor_after_100"
        has_more <- TRUE
      } else {
        expect_identical(cursor, "cursor_after_100")
        items <- roots[101:125, , drop = FALSE]
        next_cursor <- NULL
        has_more <- FALSE
      }
      list(ok = TRUE, error = NULL, server = server, jobs = items,
        next_cursor = next_cursor, has_more = has_more, schema = "root_v1",
        queue_visibility = "shared")
    },
    .studio_fetch_detail = function(conns, server, tracking_id) {
      detail_ids <<- c(detail_ids, tracking_id)
      list(ok = TRUE, error = NULL,
        status = list(tracking_id = tracking_id, state = "running",
          is_done = FALSE, kind = "analysis"),
        outputs = data.frame(name = character(0), kind = character(0),
          classification = character(0), stringsAsFactors = FALSE),
        result = list(ready = FALSE))
    },
    .package = "dsHPCClient")

  app <- dsHPCClient:::.studio_app(list(node1 = list()))
  shiny::testServer(app, {
    session$setInputs(server = "node1")
    expect_equal(nrow(snapshot()$jobs), 100L)
    expect_equal(length(unique(snapshot()$jobs$tracking_id)), 100L)
    expect_true(snapshot()$has_more)
    expect_false(target_id %in% snapshot()$jobs$tracking_id)

    session$setInputs(lookup_id = target_id, lookup = 1L)
    expect_identical(selected(), target_id)
    expect_true(detail()$ok)
    expect_identical(detail()$status$tracking_id, target_id)
    expect_identical(detail_ids, target_id)

    session$setInputs(load_more = 1L)
    expect_equal(nrow(snapshot()$jobs), 125L)
    expect_equal(length(unique(snapshot()$jobs$tracking_id)), 125L)
    expect_false(snapshot()$has_more)
    expect_true(target_id %in% snapshot()$jobs$tracking_id)
  })
})

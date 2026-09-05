.tracking_id <- function(n = 1L) {
  sprintf("trk_%08d-1111-4111-8111-%012d", n, n)
}

.tracking_page <- function(items, next_cursor = NULL, has_more = FALSE) {
  list(items = items, next_cursor = next_cursor, has_more = has_more,
    schema = "root_v1")
}

.tracking_items <- function(ids, states = rep("running", length(ids))) {
  data.frame(tracking_id = ids, state = states,
    is_done = states == "terminal", kind = "analysis",
    stringsAsFactors = FALSE)
}

test_that("shared listing follows every page by default", {
  calls <- list()
  first <- .tracking_items(c(.tracking_id(1), .tracking_id(2)))
  second <- .tracking_items(.tracking_id(3), "terminal")
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      calls[[length(calls) + 1L]] <<- expr
      cursor <- expr[[3L]]
      page <- if (is.null(cursor)) {
        .tracking_page(first, "cur_11111111-1111-4111-8111-111111111111",
          TRUE)
      } else {
        .tracking_page(second)
      }
      stats::setNames(list(page), names(conns))
    },
    .package = "DSI")

  result <- ds.hpc.list(list(site1 = list()), limit = 2L)

  expect_s3_class(result, "dshpc_result")
  expect_equal(result$per_site$site1$tracking_id,
    c(first$tracking_id, second$tracking_id))
  expect_length(calls, 2L)
  expect_true(all(vapply(calls, function(expr)
    identical(as.character(expr[[1L]]), "hpcTrackingListDS"), logical(1))))
  rendered <- paste(vapply(calls, function(expr)
    paste(deparse(expr), collapse = " "), character(1)), collapse = " ")
  expect_false(grepl("hpcListDS|hpcStudioDS|hpcSchedulerStatusDS", rendered))
})

test_that("shared listing requires unique named connections", {
  conns <- structure(list(list(), list()), names = c("site1", "site1"))
  expect_error(ds.hpc.list(conns), "unique non-empty node names",
    fixed = TRUE)
})

test_that("per-site tracking ids and cursors require unique names", {
  ids <- c(site1 = .tracking_id(), site1 = .tracking_id(2L))
  cursors <- c(
    site1 = "cur_11111111-1111-4111-8111-111111111111",
    site1 = "cur_22222222-2222-4222-8222-222222222222")
  expect_error(dsHPCClient:::.ds_tracking_id_for_site(ids, "site1"),
    "uniquely named", fixed = TRUE)
  expect_error(dsHPCClient:::.ds_tracking_cursor_for_site(cursors, "site1"),
    "uniquely named", fixed = TRUE)
})

test_that("single-page and explicit max preserve continuation", {
  calls <- list()
  page <- .tracking_page(.tracking_items(c(.tracking_id(1), .tracking_id(2))),
    "cur_22222222-2222-4222-8222-222222222222", TRUE)
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      calls[[length(calls) + 1L]] <<- expr
      stats::setNames(list(page), names(conns))
    },
    .package = "DSI")

  result <- ds.hpc.list(list(site1 = list()), limit = 2L, all = FALSE)
  expect_true(attr(result$per_site$site1, "has_more"))
  expect_equal(attr(result$per_site$site1, "next_cursor"), page$next_cursor)

  limited <- ds.hpc.list(list(site1 = list()), limit = 500L,
    max_items = 2L)
  expect_equal(as.integer(calls[[2L]][[2L]]), 2L)
  expect_equal(nrow(limited$per_site$site1), 2L)
  expect_true(attr(limited$per_site$site1, "has_more"))
})

test_that("tracking responses are reduced to the fixed public schema", {
  secret <- "/srv/cohort/patient-123.nii.gz"
  items <- .tracking_items(.tracking_id())
  items$label <- secret
  items$submitted_at <- "2026-01-01T01:02:03Z"
  items$child_job_id <- "job_private"
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      stats::setNames(list(.tracking_page(items)), names(conns))
    },
    .package = "DSI")

  result <- ds.hpc.list(list(site1 = list()))
  expect_identical(names(result$per_site$site1),
    c("tracking_id", "state", "is_done", "kind"))
  expect_false(grepl(secret,
    paste(capture.output(str(result)), collapse = "\n"), fixed = TRUE))
})

test_that("tracking normalization strips remote vector attributes", {
  items <- .tracking_items(.tracking_id())
  attr(items$tracking_id, "secret") <- "patient-id"
  attr(items$state, "secret") <- "cohort-state"
  attr(items$is_done, "secret") <- "cohort-flag"
  attr(items$kind, "secret") <- "cohort-kind"

  normalized <- dsHPCClient:::.ds_tracking_table(items)
  expect_null(attr(normalized$tracking_id, "secret"))
  expect_null(attr(normalized$state, "secret"))
  expect_null(attr(normalized$is_done, "secret"))
  expect_null(attr(normalized$kind, "secret"))

  page <- .tracking_page(items,
    structure("cur_11111111-1111-4111-8111-111111111111",
      secret = "cohort-cursor"), TRUE)
  normalized_page <- dsHPCClient:::.ds_tracking_page(page,
    expected_limit = 1L)
  expect_null(attr(normalized_page$next_cursor, "secret"))

  pending <- list(ready = FALSE,
    state = structure("running", secret = "cohort-state"),
    error = structure(NA_character_, secret = "cohort-error"))
  result <- dsHPCClient:::.ds_tracking_result(pending)
  expect_null(attr(result$state, "secret"))
  expect_null(attr(result$error, "secret"))
})

test_that("tracking kind is a closed neutral vocabulary", {
  imaging <- .tracking_items(.tracking_id())
  imaging$kind <- "imaging"
  expect_identical(dsHPCClient:::.ds_tracking_table(imaging)$kind, "imaging")

  imaging$kind <- "secret-cohort-label"
  expect_error(dsHPCClient:::.ds_tracking_table(imaging),
    "Invalid shared tracking response", fixed = TRUE)
})

test_that("malformed shared pages fail without reflecting private fields", {
  secret <- "/srv/private/cohort"
  bad <- .tracking_items(.tracking_id())
  bad$state <- secret
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      stats::setNames(list(.tracking_page(bad)), names(conns))
    },
    .package = "DSI")

  warnings <- testthat::capture_warnings(
    result <- ds.hpc.list(list(site1 = list())))
  expect_equal(nrow(result$per_site$site1), 0L)
  expect_false(any(grepl(secret, warnings, fixed = TRUE)))
  expect_identical(attr(result$per_site, "ds_errors")$site1,
    "Remote dsHPC request failed.")
})

test_that("a missing remote page is not mistaken for an empty history", {
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      stats::setNames(list(NULL), names(conns))
    },
    .package = "DSI")

  warnings <- testthat::capture_warnings(
    result <- ds.hpc.list(list(site1 = list())))
  expect_equal(nrow(result$per_site$site1), 0L)
  expect_match(warnings, "dsHPC call failed on server 'site1'", fixed = TRUE)
  expect_identical(attr(result$per_site, "ds_errors")$site1,
    "Remote dsHPC request failed.")
})

test_that("a remote page cannot exceed the requested bound", {
  oversized <- rbind(.tracking_items(.tracking_id()),
    .tracking_items(.tracking_id(2L)), .tracking_items(.tracking_id(3L)))
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      stats::setNames(list(.tracking_page(oversized)), names(conns))
    },
    .package = "DSI")

  warnings <- testthat::capture_warnings(
    result <- ds.hpc.list(list(site1 = list()), limit = 2L,
      max_items = 2L))
  expect_equal(nrow(result$per_site$site1), 0L)
  expect_match(warnings, "dsHPC call failed on server 'site1'", fixed = TRUE)
  expect_identical(attr(result$per_site, "ds_errors")$site1,
    "Remote dsHPC request failed.")
})

test_that("a terminal page cannot carry a continuation cursor", {
  page <- .tracking_page(.tracking_items(.tracking_id()),
    "cur_11111111-1111-4111-8111-111111111111", has_more = FALSE)
  expect_error(dsHPCClient:::.ds_tracking_page(page, expected_limit = 1L),
    "Invalid shared tracking response", fixed = TRUE)
})

test_that("tracking schemas reject partial and duplicate field names", {
  items <- .tracking_items(.tracking_id())
  partial_page <- list(items = items, next_cursor = NULL,
    has_more_private = FALSE, schema_private = "root_v1")
  expect_error(dsHPCClient:::.ds_tracking_page(partial_page),
    "Invalid shared tracking response", fixed = TRUE)

  duplicate_status <- c(as.list(items[1L, , drop = FALSE]),
    list(kind = "imaging"))
  expect_error(dsHPCClient:::.ds_tracking_status(duplicate_status),
    "Invalid shared tracking response", fixed = TRUE)

  expect_error(dsHPCClient:::.ds_tracking_result(list(
    readyness = FALSE, state_private = "running",
    error_private = NA_character_)),
    "Invalid shared result response", fixed = TRUE)
  expect_error(dsHPCClient:::.ds_tracking_result(list(
    readyness = TRUE, summaries_private = list(),
    available_outputs_private = list())),
    "Invalid shared result response", fixed = TRUE)
})

test_that("a ready result without summaries keeps an empty fixed schema", {
  result <- dsHPCClient:::.ds_tracking_result(list(ready = TRUE,
    summaries = list(), available_outputs = list()))
  expect_identical(result, list(ready = TRUE, summaries = list(),
    available_outputs = list()))
})

test_that("shared summaries and availability describe the same output", {
  summary <- list(name = "output_002", kind = "summary",
    value = list(n_samples = 8L))
  available <- list(name = "output_002", kind = "summary")
  expect_error(dsHPCClient:::.ds_tracking_result(list(ready = TRUE,
    summaries = list(summary), available_outputs = list())),
    "Invalid shared result response", fixed = TRUE)
  expect_error(dsHPCClient:::.ds_tracking_result(list(ready = TRUE,
    summaries = list(), available_outputs = list(available))),
    "Invalid shared result response", fixed = TRUE)
})

test_that("not-ready states carry only their canonical error", {
  expect_error(dsHPCClient:::.ds_tracking_result(list(ready = FALSE,
    state = "running", error = "Job execution failed.")),
    "Invalid shared result response", fixed = TRUE)
  expect_error(dsHPCClient:::.ds_tracking_result(list(ready = FALSE,
    state = "terminal", error = NA_character_)),
    "Invalid shared result response", fixed = TRUE)
})

test_that("listing rejects a replay of the caller supplied cursor", {
  cursor <- "cur_11111111-1111-4111-8111-111111111111"
  calls <- 0L
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      calls <<- calls + 1L
      stats::setNames(list(.tracking_page(
        .tracking_items(.tracking_id()), cursor, TRUE)), names(conns))
    },
    .package = "DSI")

  warnings <- testthat::capture_warnings(result <- ds.hpc.list(
    list(site1 = list()), cursor = cursor))
  expect_identical(calls, 1L)
  expect_match(warnings, "dsHPC call failed on server 'site1'", fixed = TRUE)
  expect_identical(attr(result$per_site, "ds_errors")$site1,
    "Remote dsHPC request failed.")

  calls <- 0L
  warnings <- testthat::capture_warnings(manual <- ds.hpc.list(
    list(site1 = list()), cursor = cursor, all = FALSE))
  expect_identical(calls, 1L)
  expect_equal(nrow(manual$per_site$site1), 0L)
  expect_match(warnings, "dsHPC call failed on server 'site1'", fixed = TRUE)
  expect_identical(attr(manual$per_site, "ds_errors")$site1,
    "Remote dsHPC request failed.")
})

test_that("status result and outputs dispatch tracking ids to new methods", {
  methods <- character(0)
  secret <- "/srv/private/result.rds"
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      method <- as.character(expr[[1L]])
      methods <<- c(methods, method)
      payload <- switch(method,
        hpcTrackingStatusDS = c(as.list(.tracking_items(
          .tracking_id(), "terminal")[1L, ]), list(path = secret)),
        hpcTrackingResultDS = list(ready = TRUE,
          summaries = list(list(name = "output_002", kind = "summary",
            value = list(n_samples = 8L), path = secret)),
          available_outputs = list(list(name = "output_002", kind = "summary",
            path = secret)), path = secret),
        hpcTrackingOutputsDS = data.frame(
          name = c("output_001", "output_002"),
          kind = c("server_object", "summary"),
          classification = c("server_reusable", "client_safe"),
          path = secret, stringsAsFactors = FALSE),
        stop("unexpected method", call. = FALSE))
      stats::setNames(list(payload), names(conns))
    },
    .package = "DSI")

  conns <- list(site1 = list())
  status <- ds.hpc.status(conns, .tracking_id())
  result <- ds.hpc.result(conns, .tracking_id())
  outputs <- ds.hpc.outputs(conns, .tracking_id())

  expect_identical(methods, c("hpcTrackingStatusDS",
    "hpcTrackingResultDS", "hpcTrackingOutputsDS"))
  expect_identical(names(status$per_site$site1),
    c("tracking_id", "state", "is_done", "kind"))
  expect_false("path" %in% names(result$per_site$site1))
  expect_false("path" %in% names(result$per_site$site1$summaries[[1L]]))
  expect_false("path" %in%
    names(result$per_site$site1$available_outputs[[1L]]))
  expect_identical(names(outputs$per_site$site1),
    c("name", "kind", "classification"))
})

test_that("tracking status must describe the requested root", {
  requested <- .tracking_id()
  returned <- .tracking_id(2L)
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      value <- as.list(.tracking_items(returned)[1L, , drop = FALSE])
      stats::setNames(list(value), names(conns))
    },
    .package = "DSI")

  warnings <- testthat::capture_warnings(
    result <- ds.hpc.status(list(site1 = list()), requested))
  expect_false("site1" %in% names(result$per_site))
  expect_match(warnings, "dsHPC call failed on server 'site1'", fixed = TRUE)
  expect_identical(attr(result$per_site, "ds_errors")$site1,
    "Remote dsHPC request failed.")
})

test_that("shared result rejects non-generic server error details", {
  secret <- "/srv/private/patient-123"
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      stats::setNames(list(list(state = "terminal", ready = FALSE,
        error = secret)), names(conns))
    },
    .package = "DSI")
  warnings <- testthat::capture_warnings(
    result <- ds.hpc.result(list(site1 = list()), .tracking_id()))
  expect_null(result$per_site$site1)
  expect_identical(attr(result$per_site, "ds_errors")$site1,
    "Remote dsHPC request failed.")
  expect_false(any(grepl(secret, warnings, fixed = TRUE)))
})

test_that("private bearers retain capability endpoint compatibility", {
  methods <- character(0)
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      method <- as.character(expr[[1L]])
      methods <<- c(methods, method)
      value <- switch(method,
        hpcStatusDS = list(state = "FINISHED", is_done = TRUE),
        hpcResultDS = list(ready = TRUE),
        hpcOutputsDS = data.frame(name = character(), kind = character()),
        stop("unexpected method", call. = FALSE))
      stats::setNames(list(value), names(conns))
    },
    .package = "DSI")

  conns <- list(site1 = list())
  ds.hpc.status(conns, "B64:private-bearer")
  ds.hpc.result(conns, "B64:private-bearer")
  ds.hpc.outputs(conns, "B64:private-bearer")
  ds.hpc.status(conns, "trk_private_symbol")
  expect_identical(methods, c("hpcStatusDS", "hpcResultDS", "hpcOutputsDS",
    "hpcStatusDS"))
})

test_that("shared output assignment creates only server-side references", {
  observed <- NULL
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, async = TRUE,
                                      success = NULL, error = NULL,
                                      errors.print = FALSE) {
      observed <<- list(conns = conns, symbol = symbol, expr = expr)
      for (site in names(conns)) success(site)
      invisible(NULL)
    },
    .package = "DSI")

  conns <- list(site1 = list(), site2 = list())
  ids <- c(site1 = .tracking_id(1), site2 = .tracking_id(2))
  expect_true(ds.hpc.load_output(conns, ids, "output_001",
    symbol = "shared_features"))
  expect_equal(observed$symbol, "shared_features")
  expect_identical(names(observed$expr), names(conns))
  expect_true(all(vapply(observed$expr, function(expr)
    identical(as.character(expr[[1L]]), "hpcTrackingAssignOutputDS"),
    logical(1))))
  expect_equal(as.character(observed$expr$site1[[2L]]), ids[["site1"]])
  expect_false(any(grepl("hpcLoadOutputDS", vapply(observed$expr,
    function(expr) paste(deparse(expr), collapse = " "), character(1)))))
})

test_that("shared output assignment derives a valid default symbol", {
  observed <- NULL
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, async = TRUE,
                                      success = NULL, error = NULL,
                                      errors.print = FALSE) {
      observed <<- symbol
      for (site in names(conns)) success(site)
      invisible(NULL)
    },
    .package = "DSI")

  expect_true(ds.hpc.load_output(list(site1 = list()), .tracking_id(),
    "output_001"))
  expect_identical(observed, "output_001")

  expect_true(ds.hpc.load_output(list(site1 = list()), .tracking_id(),
    "output_002"))
  expect_identical(observed, "output_002")
  expect_error(ds.hpc.load_output(list(site1 = list()), .tracking_id(),
    "features"), "valid shared output alias")
  expect_error(ds.hpc.load_output(list(site1 = list()), .tracking_id(),
    "output_001", symbol = "TRUE"), "symbol beginning with a letter")
})

test_that("shared output assignment does not reflect provider failures", {
  secret <- "/srv/private/patient-output.rds"
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, async = TRUE,
                                      success = NULL, error = NULL,
                                      errors.print = FALSE) {
      error(names(conns)[1L], paste("failed at", secret))
      warning(paste("provider", secret), call. = FALSE)
      invisible(NULL)
    },
    .package = "DSI")

  message <- tryCatch({
    ds.hpc.load_output(list(site1 = list()), .tracking_id(), "output_001")
    NULL
  }, error = conditionMessage)
  expect_match(message, "Shared output assignment failed", fixed = TRUE)
  expect_false(grepl(secret, message, fixed = TRUE))
})

test_that("shared output metadata uses only the fixed public projection", {
  valid <- data.frame(name = c("output_001", "output_002"),
    kind = c("server_object", "summary"),
    classification = c("server_reusable", "client_safe"),
    stringsAsFactors = FALSE)
  expect_identical(dsHPCClient:::.ds_tracking_outputs(valid), valid)

  attributed <- valid
  attr(attributed$name, "secret") <- "cohort-output"
  attr(attributed$kind, "secret") <- "cohort-kind"
  normalized <- dsHPCClient:::.ds_tracking_outputs(attributed)
  expect_null(attr(normalized$name, "secret"))
  expect_null(attr(normalized$kind, "secret"))

  bad_name <- valid
  bad_name$name[1L] <- "patient_features"
  expect_error(dsHPCClient:::.ds_tracking_outputs(bad_name),
    "Invalid shared output response", fixed = TRUE)

  bad_kind <- valid
  bad_kind$kind[1L] <- "radiomics"
  expect_error(dsHPCClient:::.ds_tracking_outputs(bad_kind),
    "Invalid shared output response", fixed = TRUE)

  bad_pair <- valid
  bad_pair$classification[1L] <- "client_safe"
  expect_error(dsHPCClient:::.ds_tracking_outputs(bad_pair),
    "Invalid shared output response", fixed = TRUE)

  swapped_aliases <- valid
  swapped_aliases$name <- rev(swapped_aliases$name)
  expect_error(dsHPCClient:::.ds_tracking_outputs(swapped_aliases),
    "Invalid shared output response", fixed = TRUE)

  too_many <- rbind(valid, valid[1L, , drop = FALSE])
  expect_error(dsHPCClient:::.ds_tracking_outputs(too_many),
    "Invalid shared output response", fixed = TRUE)
})

test_that("shared summaries reject values outside the closed schema", {
  good <- list(ready = TRUE,
    summaries = list(list(name = "output_002", kind = "summary",
      value = list(n_samples = 8L))),
    available_outputs = list(list(name = "output_002", kind = "summary")))
  expect_true(dsHPCClient:::.ds_tracking_result(good)$ready)

  named_containers <- good
  names(named_containers$summaries) <- "patient-007"
  names(named_containers$available_outputs) <- "secret-cohort"
  normalized <- dsHPCClient:::.ds_tracking_result(named_containers)
  expect_null(names(normalized$summaries))
  expect_null(names(normalized$available_outputs))

  wrong_alias <- good
  wrong_alias$summaries[[1L]]$name <- "output_001"
  expect_error(dsHPCClient:::.ds_tracking_result(wrong_alias),
    "Invalid shared result response", fixed = TRUE)

  secret_key <- good
  secret_key$summaries[[1L]]$value <- list(patient_id = 8L)
  expect_error(dsHPCClient:::.ds_tracking_result(secret_key),
    "Invalid shared result response", fixed = TRUE)

  string_value <- good
  string_value$summaries[[1L]]$value <- list(n_samples = "patient-123")
  expect_error(dsHPCClient:::.ds_tracking_result(string_value),
    "Invalid shared result response", fixed = TRUE)

  attributed <- good
  attributed$summaries[[1L]]$value <- list(n_samples = structure(8L,
    patient = "123"))
  expect_error(dsHPCClient:::.ds_tracking_result(attributed),
    "Invalid shared result response", fixed = TRUE)

  attributed_names <- good
  value <- list(8L)
  attributes(value) <- list(names = structure("n_samples",
    patient = "123"))
  attributed_names$summaries[[1L]]$value <- value
  expect_error(dsHPCClient:::.ds_tracking_result(attributed_names),
    "Invalid shared result response", fixed = TRUE)
})

test_that("logs remain unavailable for public tracking ids", {
  calls <- 0L
  testthat::local_mocked_bindings(
    datashield.aggregate = function(...) {
      calls <<- calls + 1L
      stop("must not be called")
    },
    .package = "DSI")
  expect_error(ds.hpc.logs(list(site1 = list()), .tracking_id()),
    "not available")
  expect_equal(calls, 0L)
})

test_that("summary counts logical roots, not server execution metadata", {
  items <- .tracking_items(c(.tracking_id(1), .tracking_id(2),
    .tracking_id(3)), c("queued", "running", "terminal"))
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      stats::setNames(list(.tracking_page(items)), names(conns))
    },
    .package = "DSI")
  summary <- ds.hpc.summary(list(site1 = list()))
  expect_identical(names(summary$per_site$site1), c("state", "roots"))
  expect_equal(summary$per_site$site1$roots, c(1L, 1L, 1L))
})

.capture_sensitive_output <- function(code) {
  messages <- character(0)
  value <- NULL
  stdout <- withCallingHandlers(
    utils::capture.output(value <- force(code), type = "output"),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, text = c(stdout, messages))
}

test_that("sensitive aggregate calls suppress DSI expression progress", {
  bearer <- "B64:CAPABILITY_MARKER_DO_NOT_PRINT"
  admin_key <- "ADMIN_MARKER_DO_NOT_PRINT"
  admin_transport <- dsHPCClient:::.ds_encode(list(.admin_key = admin_key))
  observed <- list()

  old_options <- options(
    datashield.progress = TRUE,
    datashield.errors.print = TRUE,
    progress_enabled = TRUE
  )
  on.exit(options(old_options), add = TRUE)

  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      observed[[length(observed) + 1L]] <<- c(
        datashield_progress = getOption("datashield.progress"),
        errors_print = getOption("datashield.errors.print"),
        progress_enabled = getOption("progress_enabled")
      )
      rendered <- paste(deparse(expr), collapse = " ")
      if (isTRUE(getOption("progress_enabled", TRUE))) {
        cat("DSI initial progress\n")
      }
      if (isTRUE(getOption("datashield.progress", TRUE))) {
        cat("DSI stdout progress: ", rendered, "\n", sep = "")
        message("DSI message progress: ", rendered)
      }

      method <- as.character(expr[[1L]])
      payload <- switch(method,
        hpcStatusDS = list(state = "FINISHED", is_done = TRUE),
        hpcOutputsDS = data.frame(
          name = "summary", kind = "summary", safe_for_client = TRUE,
          size_bytes = 1, stringsAsFactors = FALSE),
        hpcLogsDS = character(0),
        hpcResultDS = list(ready = TRUE),
        hpcJobReferenceDS = "B64:returned-reference",
        hpcAdminListDS = data.frame(
          job_id = character(0), state = character(0),
          stringsAsFactors = FALSE),
        hpcAdminCancelDS = list(state = "CANCELLED"),
        stop("unexpected aggregate method", call. = FALSE)
      )
      stats::setNames(list(payload), names(conns))
    },
    .package = "DSI"
  )

  conns <- list(site = list())
  calls <- list(
    status = function() ds.hpc.status(conns, bearer),
    polling = function() ds.hpc.wait(
      conns, bearer, timeout = 1, poll_interval = 0),
    outputs = function() ds.hpc.outputs(conns, bearer),
    logs = function() ds.hpc.logs(conns, bearer),
    result = function() ds.hpc.result(conns, bearer),
    job_reference = function() ds.hpc.job_id(conns, bearer),
    admin_list = function() ds.hpc.admin.list(conns, admin_key),
    admin_cancel = function() ds.hpc.admin.cancel(
      conns, bearer, admin_key)
  )

  for (call_name in names(calls)) {
    captured <- .capture_sensitive_output(calls[[call_name]]())
    expect_false(any(grepl(bearer, captured$text, fixed = TRUE)),
      info = call_name)
    expect_false(any(grepl(admin_transport, captured$text, fixed = TRUE)),
      info = call_name)
    expect_false(any(grepl(admin_key, captured$text, fixed = TRUE)),
      info = call_name)
    expect_false(any(grepl("DSI", captured$text, fixed = TRUE)),
      info = call_name)
    expect_identical(getOption("datashield.progress"), TRUE,
      info = call_name)
    expect_identical(getOption("datashield.errors.print"), TRUE,
      info = call_name)
    expect_identical(getOption("progress_enabled"), TRUE,
      info = call_name)
  }

  expect_true(length(observed) >= length(calls))
  expect_true(all(vapply(observed, function(x) all(!x), logical(1))))
})

test_that("remote aggregate failures do not reflect sensitive details", {
  bearer <- "B64:REMOTE_ERROR_SECRET_MARKER"
  raw_error <- paste("remote failure at /srv/private/jobs", bearer)

  old_options <- options(
    datashield.progress = TRUE,
    datashield.errors.print = TRUE,
    progress_enabled = TRUE
  )
  on.exit(options(old_options), add = TRUE)

  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      if (isTRUE(getOption("datashield.progress", TRUE))) {
        message("Aggregating ", paste(deparse(expr), collapse = " "))
      }
      stop(raw_error, call. = FALSE)
    },
    .package = "DSI"
  )

  direct_messages <- character(0)
  direct_error <- withCallingHandlers(
    tryCatch(ds.hpc.status(list(site = list()), bearer), error = identity),
    message = function(m) {
      direct_messages <<- c(direct_messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_s3_class(direct_error, "error")
  expect_identical(conditionMessage(direct_error),
    "Remote dsHPC request failed.")
  expect_false(any(grepl(bearer, direct_messages, fixed = TRUE)))

  safe_warnings <- character(0)
  safe_messages <- character(0)
  safe_result <- withCallingHandlers(
    ds.hpc.outputs(list(site = list()), bearer),
    warning = function(w) {
      safe_warnings <<- c(safe_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      safe_messages <<- c(safe_messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  reflected <- c(
    safe_warnings,
    safe_messages,
    unlist(attr(safe_result$per_site, "ds_errors"), use.names = FALSE)
  )
  expect_false(any(grepl(bearer, reflected, fixed = TRUE)))
  expect_false(any(grepl("/srv/private", reflected, fixed = TRUE)))
  expect_match(safe_warnings, "site", fixed = TRUE)
  expect_identical(getOption("datashield.progress"), TRUE)
  expect_identical(getOption("datashield.errors.print"), TRUE)
  expect_identical(getOption("progress_enabled"), TRUE)
})

test_that("ds.hpc.job_id uses the explicit reference method", {
  methods <- character(0)
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      methods <<- c(methods, as.character(expr[[1]]))
      server <- names(conns)[1]
      stats::setNames(list(if (identical(server, "site1"))
        "B64:opaque-reference" else 7L), server)
    },
    .package = "DSI")

  ids <- ds.hpc.job_id(list(site1 = 1, site2 = 2), "jobA")
  expect_equal(ids[["site1"]], "B64:opaque-reference")
  expect_true(is.na(ids[["site2"]]))
  expect_identical(methods, rep("hpcJobReferenceDS", 2L))
})

test_that("ds.hpc.job_id returns NA for a server-side resolution failure", {
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      server <- names(conns)[1]
      if (identical(server, "site2")) stop("remote detail", call. = FALSE)
      stats::setNames(list("B64:site1-reference"), server)
    },
    .package = "DSI")

  ids <- ds.hpc.job_id(list(site1 = 1, site2 = 2), "jobA")
  expect_equal(ids[["site1"]], "B64:site1-reference")
  expect_true(is.na(ids[["site2"]]))
})

test_that("named bearers are routed to their matching servers", {
  observed <- data.frame(method = character(0), server = character(0),
    reference = character(0), stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      server <- names(conns)[1]
      method <- as.character(expr[[1L]])
      observed <<- rbind(observed, data.frame(method = method,
        server = server, reference = as.character(expr[[2L]]),
        stringsAsFactors = FALSE))
      value <- switch(method,
        hpcStatusDS = list(state = "FINISHED", is_done = TRUE),
        hpcResultDS = list(ready = TRUE),
        hpcOutputsDS = data.frame(name = character(0), kind = character(0)),
        hpcLogsDS = character(0),
        hpcAdminCancelDS = list(state = "CANCELLED"),
        stop("unexpected method", call. = FALSE))
      stats::setNames(list(value), server)
    },
    .package = "DSI")

  conns <- list(site1 = 1, site2 = 2)
  refs <- c(site1 = "B64:site1-reference", site2 = "B64:site2-reference")
  ds.hpc.status(conns, refs)
  ds.hpc.result(conns, refs)
  suppressWarnings(ds.hpc.outputs(conns, refs))
  suppressWarnings(ds.hpc.logs(conns, refs))
  suppressWarnings(ds.hpc.admin.cancel(conns, refs, "admin"))

  expected <- c(site1 = "B64:site1-reference", site2 = "B64:site2-reference")
  expect_true(nrow(observed) > 0L)
  for (server in names(expected)) {
    expect_true(all(observed$reference[observed$server == server] == expected[[server]]))
  }
})

test_that("status and result retain successful sites when one site fails", {
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      server <- names(conns)[1]
      if (identical(server, "site2")) stop("private remote detail", call. = FALSE)
      value <- if (identical(as.character(expr[[1L]]), "hpcStatusDS")) {
        list(state = "FINISHED", is_done = TRUE)
      } else {
        list(ready = TRUE)
      }
      stats::setNames(list(value), server)
    },
    .package = "DSI")

  conns <- list(site1 = 1, site2 = 2)
  refs <- c(site1 = "B64:site1-reference", site2 = "B64:site2-reference")
  status <- suppressWarnings(ds.hpc.status(conns, refs))
  result <- suppressWarnings(ds.hpc.result(conns, refs))

  expect_equal(status$per_site$site1$state, "FINISHED")
  expect_true(status$per_site$site2 |> is.null())
  expect_identical(attr(status$per_site, "ds_errors")$site2,
    "Remote dsHPC request failed.")
  expect_true(result$per_site$site1$ready)
  expect_true(result$per_site$site2 |> is.null())
  expect_identical(attr(result$per_site, "ds_errors")$site2,
    "Remote dsHPC request failed.")
})

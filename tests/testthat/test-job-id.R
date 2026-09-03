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

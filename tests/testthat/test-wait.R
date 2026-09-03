test_that("ds.hpc.wait never prints a transferable bearer", {
  bearer <- "eyJqb2JfaWQiOiJqb2Jfc2VjcmV0IiwiY2FwYWJpbGl0eSI6InNlY3JldCJ9"
  local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      stats::setNames(list(list(state = "FINISHED", is_done = TRUE)), names(conns))
    },
    .package = "DSI"
  )
  local_mocked_bindings(
    ds.hpc.status = function(conns, job_id) {
      dsHPCClient:::dshpc_result(
        per_site = list(site = list(state = "FINISHED", is_done = TRUE)),
        meta = list(servers = "site"))
    },
    .package = "dsHPCClient"
  )

  output <- testthat::capture_messages(
    ds.hpc.wait(list(site = NULL), bearer, timeout = 1, poll_interval = 0))
  expect_false(any(grepl(bearer, output, fixed = TRUE)))
  expect_true(any(grepl("Waiting for job reference", output, fixed = TRUE)))
})

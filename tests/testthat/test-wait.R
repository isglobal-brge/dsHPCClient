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

test_that("ds.hpc.wait polls each server with its named bearer", {
  observed <- list()
  local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      server <- names(conns)[1]
      observed[[server]] <<- c(observed[[server]], as.character(expr[[2L]]))
      stats::setNames(list(list(state = "FINISHED", is_done = TRUE)), server)
    },
    .package = "DSI"
  )
  refs <- c(site1 = "B64:one", site2 = "B64:two")
  testthat::capture_messages(ds.hpc.wait(list(site1 = 1, site2 = 2), refs,
    timeout = 1, poll_interval = 0))

  expect_true(all(observed$site1 == refs[["site1"]]))
  expect_true(all(observed$site2 == refs[["site2"]]))
})

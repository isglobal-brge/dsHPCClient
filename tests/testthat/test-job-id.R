test_that("ds.hpc.job_id resolves per-site job ids via status", {
  testthat::local_mocked_bindings(
    ds.hpc.status = function(conns, job_id) dsHPCClient:::dshpc_result(
      per_site = list(
        site1 = list(job_id = "job_123", state = "PENDING"),
        site2 = list(state = "FAILED"))),
    .package = "dsHPCClient")

  ids <- ds.hpc.job_id(list(site1 = 1, site2 = 2), "jobA")
  expect_equal(ids[["site1"]], "job_123")
  expect_true(is.na(ids[["site2"]]))
})

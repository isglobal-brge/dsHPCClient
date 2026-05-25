test_that("exported namespace is observation and admin only", {
  expected <- c(
    "ds.hpc.admin.cancel", "ds.hpc.admin.list",
    "ds.hpc.capabilities", "ds.hpc.list", "ds.hpc.logs",
    "ds.hpc.outputs", "ds.hpc.result", "ds.hpc.scheduler_status",
    "ds.hpc.status", "ds.hpc.studio", "ds.hpc.studio_data",
    "ds.hpc.summary", "ds.hpc.wait"
  )
  expect_setequal(getNamespaceExports("dsHPCClient"), expected)
})

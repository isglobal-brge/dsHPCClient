test_that("exported namespace is observation and admin only", {
  expected <- c(
    "ds.hpc.admin.cancel", "ds.hpc.admin.list",
    "ds.hpc.capabilities", "ds.hpc.job_id", "ds.hpc.list", "ds.hpc.logs",
    "ds.hpc.outputs", "ds.hpc.result", "ds.hpc.scheduler_status",
    "ds.hpc.status", "ds.hpc.studio", "ds.hpc.studio_data",
    "ds.hpc.summary", "ds.hpc.unit.destroy", "ds.hpc.unit.init",
    "ds.hpc.wait"
  )
  expect_setequal(getNamespaceExports("dsHPCClient"), expected)
})

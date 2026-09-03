test_that("Studio snapshot normalization keeps per-server errors isolated", {
  snap <- dsHPCClient:::.studio_error_snapshot("node2",
    "could not find function hpcStudioDS")

  expect_false(snap$ok)
  expect_equal(snap$server, "node2")
  expect_match(snap$error, "hpcStudioDS")
  expect_s3_class(snap$jobs, "data.frame")
  expect_equal(nrow(snap$jobs), 0L)
})

test_that("Studio DAG SVG renders nodes and edges", {
  nodes <- data.frame(
    job_id = "job_a",
    step_index = c(1L, 2L, 3L),
    node_id = c("resolve", "features", "summary"),
    type = c("emit", "run", "safe_summary"),
    plane = c("session", "artifact", "session"),
    runner = c("", "pyradiomics", ""),
    state = c("done", "running", "pending"),
    stringsAsFactors = FALSE)
  edges <- data.frame(
    job_id = "job_a",
    from_step = c(1L, 2L),
    to_step = c(2L, 3L),
    from_node = c("resolve", "features"),
    to_node = c("features", "summary"),
    input_name = c("", "features"),
    stringsAsFactors = FALSE)

  svg <- dsHPCClient:::.studio_dag_svg(nodes, edges)
  expect_match(svg, "<svg", fixed = TRUE)
  expect_match(svg, "resolve", fixed = TRUE)
  expect_match(svg, "features", fixed = TRUE)
  expect_match(svg, "marker-end", fixed = TRUE)
})

test_that("Studio tables include display name and safe output metadata", {
  jobs <- data.frame(
    state = "RUNNING",
    name = "Cohort A radiomics",
    job_id = "job_a",
    scope = "mine",
    label = "dsImaging",
    progress = "2/3",
    elapsed_seconds = 75,
    submitted_at = "2026-05-25T10:00:00.000Z",
    stringsAsFactors = FALSE)
  out <- dsHPCClient:::.studio_jobs_table(jobs)
  expect_equal(out$name, "Cohort A radiomics")

  outputs <- data.frame(
    job_id = "job_a",
    step_index = 1L,
    name = "summary",
    kind = "summary",
    safe_for_client = TRUE,
    size_bytes = 2048,
    created_at = "2026-05-25T10:01:00.000Z",
    stringsAsFactors = FALSE)
  rendered <- dsHPCClient:::.studio_outputs_table(outputs)
  expect_equal(names(rendered), c("step", "name", "kind", "safe", "size",
    "created"))
  expect_equal(rendered$size, "2 KB")
})

test_that("analyst Studio endpoints are retired before any aggregate call", {
  conns <- list(node1 = list(), node2 = list())
  calls <- 0L

  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      calls <<- calls + 1L
      stop("must not be called")
    },
    .package = "DSI"
  )

  expect_error(ds.hpc.studio_data(conns, server = "node1"), "retired")
  expect_error(ds.hpc.studio(conns), "retired")
  expect_error(ds.hpc.list(conns), "retired")
  expect_error(ds.hpc.scheduler_status(conns), "retired")
  expect_equal(calls, 0L)
})

test_that("Studio admin cancellation sends only selected job and server", {
  conns <- list(node1 = list(), node2 = list())

  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr) {
      expect_equal(names(conns), "node1")
      expect_equal(as.character(expr[[1]]), "hpcAdminCancelDS")
      expect_equal(as.character(expr[[2]]), "job_x")
      list(node1 = list(job_id = "job_x", state = "CANCELLED"))
    },
    .package = "DSI"
  )

  res <- dsHPCClient:::.studio_cancel_one(conns, "node1", "job_x",
    "admin-secret")
  expect_true(res$ok)
  expect_equal(res$result$state, "CANCELLED")
})

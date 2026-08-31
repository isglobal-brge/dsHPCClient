test_that("dshpc_result creation and printing", {
  result <- dsHPCClient:::dshpc_result(
    per_site = list(
      node1 = list(state = "RUNNING", step_index = 2L, total_steps = 3L),
      node2 = list(state = "FINISHED", step_index = 3L, total_steps = 3L)
    )
  )
  expect_s3_class(result, "dshpc_result")
  expect_output(print(result), "node1: RUNNING")
  expect_output(print(result), "node2: FINISHED")
})

test_that("dshpc_result $ accessor works", {
  result <- dsHPCClient:::dshpc_result(
    per_site = list(node1 = list(value = 42))
  )
  expect_equal(result$node1$value, 42)
  expect_true(is.list(result$per_site))
})

test_that("dshpc_result as.data.frame returns empty for non-df", {
  result <- dsHPCClient:::dshpc_result(
    per_site = list(node1 = list(state = "DONE"))
  )
  df <- as.data.frame(result)
  expect_true(is.data.frame(df))
})

test_that("print renders a jobs data.frame as a table", {
  df <- data.frame(
    job_id = c("job_1", "job_2"),
    state = c("FINISHED", "RUNNING"),
    name = c("a", "b"),
    label = c("demo", "demo"),
    submitted_at = c("t1", "t2"),
    progress = c("2/2", "1/2"),
    stringsAsFactors = FALSE)
  result <- dsHPCClient:::dshpc_result(per_site = list(site1 = df))
  expect_output(print(result), "2 job\\(s\\)")
  expect_output(print(result), "job_1")
  expect_output(print(result), "RUNNING")
  # Regression: states used to be cat()ed concatenated ("FINISHEDRUNNING")
  out <- paste(capture.output(print(result)), collapse = "\n")
  expect_false(grepl("FINISHEDRUNNING", out, fixed = TRUE))
})

test_that("print renders an empty jobs data.frame as (no jobs)", {
  df <- data.frame(job_id = character(0), state = character(0),
    name = character(0), label = character(0),
    submitted_at = character(0), progress = character(0),
    stringsAsFactors = FALSE)
  result <- dsHPCClient:::dshpc_result(per_site = list(site1 = df))
  expect_output(print(result), "(no jobs)", fixed = TRUE)
})

test_that("print renders outputs as name/kind/size rows", {
  df <- data.frame(
    name = c("result.csv", "note"),
    kind = c("artifact_file", "emit_value"),
    safe_for_client = c(1L, 1L),
    size_bytes = c(136L, 71L),
    stringsAsFactors = FALSE)
  result <- dsHPCClient:::dshpc_result(per_site = list(site1 = df))
  expect_output(print(result), "2 output\\(s\\)")
  expect_output(print(result), "result.csv")
  expect_output(print(result), "artifact_file")
  expect_output(print(result), "136 B")
})

test_that("print renders log lines", {
  result <- dsHPCClient:::dshpc_result(
    per_site = list(site1 = c("[step_001/stdout.log] hello", "line two")))
  expect_output(print(result), "2 log line\\(s\\)")
  expect_output(print(result), "hello")
  expect_output(print(result), "line two")
})

test_that("print renders per-server ds_errors", {
  per_site <- list(site1 = list(state = "FINISHED"))
  attr(per_site, "ds_errors") <- list(site2 = "Job not found.")
  result <- dsHPCClient:::dshpc_result(per_site = per_site)
  expect_output(print(result), "Errors:")
  expect_output(print(result), "site2: Job not found.", fixed = TRUE)
})

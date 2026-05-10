test_that("ds_job creates valid job object", {
  job <- ds_job(
    steps = list(
      ds_step_resolve_dataset("dataset.v1"),
      ds_step_run_artifact("pyradiomics"),
      ds_step_safe_summary()
    ),
    resource_class = "cpu_heavy"
  )
  expect_s3_class(job, "dshpc_job")
  expect_equal(length(job$steps), 3L)
  expect_equal(job$resource_class, "cpu_heavy")
})

test_that("ds_job rejects empty steps", {
  expect_error(ds_job(steps = list()), "at least one step")
})

test_that("ds_job rejects non-step objects", {
  expect_error(
    ds_job(steps = list(list(type = "fake"))),
    "not a dshpc_step"
  )
})

test_that("ds_job strips S3 classes from steps for serialization", {
  job <- ds_job(steps = list(ds_step_emit("out")))
  # Steps in the job should be plain lists (no dshpc_step class)
  expect_false(inherits(job$steps[[1]], "dshpc_step"))
})

test_that("ds_job with publish config", {
  job <- ds_job(
    steps = list(ds_step_emit("out")),
    publish = list(dataset_id = "ds.v1", asset_name = "features")
  )
  expect_equal(job$publish$dataset_id, "ds.v1")
})

test_that("job print works", {
  job <- ds_job(
    steps = list(
      ds_step_resolve_dataset("ds"),
      ds_step_run_artifact("pyradiomics")
    )
  )
  expect_output(print(job), "dshpc_job")
  expect_output(print(job), "Steps: 2")
})

test_that("ds_job accepts DAG pipelines", {
  pipeline <- ds_pipeline(list(
    ds_pipeline_node("resolve", ds_step_resolve_dataset("study.v1")),
    ds_pipeline_node("extract", ds_step_run_artifact("dummy_runner"),
      inputs = "resolve"),
    ds_pipeline_node("summary", ds_step_safe_summary(), inputs = "extract")
  ))

  job <- ds_job(pipeline = pipeline, label = "dag-demo")
  expect_s3_class(job, "dshpc_job")
  expect_equal(length(job$dag$nodes), 3L)
  expect_null(job$steps)
  expect_output(print(job), "DAG nodes: 3")
})

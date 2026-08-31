# dsHPCClient

dsHPCClient is the analyst-facing observability and administration client for
dsHPC. It provides a cross-domain view of durable jobs that have already been
submitted by domain packages, plus admin-key-gated cancellation for operators.

## For Analysts

Use dsHPCClient to inspect and monitor jobs:

- `ds.hpc.list()`, `ds.hpc.status()`, `ds.hpc.wait()`, `ds.hpc.summary()`
- `ds.hpc.outputs()`, `ds.hpc.logs()`, `ds.hpc.result()`
- `ds.hpc.capabilities()`, `ds.hpc.scheduler_status()`
- `ds.hpc.studio()`, `ds.hpc.studio_data()`
- `ds.hpc.admin.list()`, `ds.hpc.admin.cancel()` when the server admin key is
  configured

Submission, pipeline composition and output loading are mediated by domain
packages. For imaging workflows, use the `ds.imaging.*` functions from
dsImagingClient rather than constructing dsHPC jobs directly.

## Symbol-Based Submission Pattern

When a deployment allowlists `hpcSubmitDS` for direct DSI use, submission
happens through `datashield.assign()`, which returns nothing to the analyst --
the submission handle (including the job id) lives server-side in the
assigned symbol:

```r
DSI::datashield.assign(conns, "jobA", call("hpcSubmitDS", spec_enc))

# Resolve the per-server job ids from the symbol:
ids <- ds.hpc.job_id(conns, "jobA")

# Job ids differ per server; monitor with the id (or keep using the symbol):
ds.hpc.status(conns, "jobA")
ds.hpc.wait(conns, ids[["site1"]])
ds.hpc.outputs(conns, ids[["site1"]])
ds.hpc.logs(conns, ids[["site1"]])
```

Deduplication semantics: servers deduplicate byte-identical specs against
FINISHED/PUBLISHED jobs only; concurrent identical submissions both run.

## For Domain Package Developers

Domain packages should expose their own DataSHIELD methods and compose dsHPC
workflows server-side through the dsHPC R API. Spec builders, if desired, live
in the server-side package as internal helpers such as `dsHPC:::ds_job()` and
`dsHPC:::ds_step_run_artifact()`.

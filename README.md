# dsHPCClient

dsHPCClient provides capability-scoped monitoring of durable jobs submitted by
domain packages, plus admin-key-gated operations for node operators. It does
not enumerate another workflow's jobs or expose scheduler topology.

## For Analysts

Use dsHPCClient to inspect and monitor a job whose opaque symbol you already
hold:

- `ds.hpc.status()`, `ds.hpc.wait()`, `ds.hpc.job_id()`
- `ds.hpc.outputs()`, `ds.hpc.logs()`, `ds.hpc.result()`
- `ds.hpc.capabilities()`
- `ds.hpc.admin.list()`, `ds.hpc.admin.cancel()` when the server admin key is
  configured

`ds.hpc.list()`, `ds.hpc.summary()`, `ds.hpc.scheduler_status()`, and Studio
are retained as compatibility stubs and fail before making a DataSHIELD call.

Submission, pipeline composition and output loading are mediated by domain
packages. For imaging workflows, use the `ds.imaging.*` functions from
dsImagingClient rather than constructing dsHPC jobs directly.

## Domain-mediated submission pattern

Submission happens through a domain package. If that package exposes the raw
dsHPC capability as its assigned symbol, it can be monitored here. Domains may
instead wrap the job in a stricter capability of their own. dsImaging does so,
therefore its workflows must be monitored and destroyed with
`ds.imaging.workflow.status()` and `ds.imaging.workflow.destroy()`:

```r
workflow <- dsImagingClient::ds.imaging.qc.metrics(
  conns, handle = "img", symbol = "qc_job"
)

dsImagingClient::ds.imaging.workflow.status(conns, workflow)
dsImagingClient::ds.imaging.workflow.destroy(conns, workflow)
```

Only disclosure-safe output kinds explicitly marked by the domain package are
available through these calls. Call `ds.hpc.job_id()` only when an intentional
asynchronous reconnection requires a portable bearer; routine status and result
objects do not contain it. A raw internal job identifier is not sufficient for
access.

## For Domain Package Developers

Domain packages should expose their own DataSHIELD methods and compose dsHPC
workflows server-side through `dsHPC::hpcSubmitInternal()`. The generic legacy
`hpcSubmitDS()` and `hpcLoadOutputDS()` entry points always fail, including on
deployments whose persisted allowlist still mentions them.

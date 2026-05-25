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

## For Domain Package Developers

The historical job constructors and submission helpers remain available as
internal compatibility helpers, but they are no longer exported for direct
analyst use and emit deprecation warnings when called via `dsHPCClient:::`.
Domain packages should expose their own DataSHIELD methods and compose dsHPC
workflows server-side through the dsHPC R API.

# dsHPCClient

dsHPCClient provides disclosure-safe shared tracking of durable analyses,
server-side reuse of their outputs, capability-scoped compatibility for
private jobs, and admin-key-gated operations for node operators. It never
exposes execution children, patient-level artifacts, logs or scheduler
topology through the shared analyst surface.

## For Analysts

The normal workflow needs no bearer token. List the shared logical analyses on
each node, then query one by its public tracking id:

```r
jobs <- ds.hpc.list(conns)
id <- jobs$per_site$opal_site$tracking_id[[1]]

ds.hpc.status(conns["opal_site"], id)
ds.hpc.outputs(conns["opal_site"], id)
ds.hpc.result(conns["opal_site"], id)
```

`ds.hpc.list()` follows every server page by default, so its default page size
of 100 is not a historical limit. For an intentionally bounded request use
`max_items`; for manual paging use `all = FALSE` and the `next_cursor`
attribute.

The analyst surface includes:

- `ds.hpc.status()`, `ds.hpc.wait()`, `ds.hpc.job_id()`
- `ds.hpc.list()`, `ds.hpc.summary()`, `ds.hpc.studio()`
- `ds.hpc.outputs()`, `ds.hpc.result()`, `ds.hpc.load_output()`
- `ds.hpc.logs()` for capability-authorized private jobs only
- `ds.hpc.capabilities()`
- `ds.hpc.admin.list()`, `ds.hpc.admin.cancel()` when the server admin key is
  configured

`ds.hpc.scheduler_status()` remains retired. Operational topology is available
only through node administration.

Studio uses the same root-level contract:

```r
ds.hpc.studio(conns)
```

It can page through shared roots, find a known tracking id directly, show
approved results and identify reusable outputs. It deliberately has no raw
labels, timestamps, progress counts, child jobs, retries, logs, cache-hit
signals, DAG internals, cancellation controls or scheduler panels. If a server
does not advertise the expected versioned capabilities, Studio fails closed
with a generic availability message.

## Reuse outputs without downloading them

Outputs classified `server_reusable` can become opaque objects in the server R
session. The public catalogue deliberately uses fixed ordinal aliases instead
of domain or cohort-derived names: `output_001` is the reusable server object
and `output_002`, when present, is the closed count-only summary.

```r
ds.hpc.load_output(
  conns["opal_site"], id,
  output_name = "output_001",
  symbol = "shared_features"
)
```

`shared_features` is a `dshpc_output_reference`, not the feature table, file or
path. A trusted domain package can use that reference as a later pipeline
input. Nothing is returned to the client and every later client-facing result
must pass disclosure control again.

Submission, pipeline composition and output loading are mediated by domain
packages. For imaging workflows, use the `ds.imaging.*` functions from
dsImagingClient rather than constructing dsHPC jobs directly.

To select an administrator-managed unit for later domain submissions, pass one
opaque Resource name per site. This uses DSI generics and supports mixed Opal
and Armadillo connections:

```r
ds.hpc.unit.init(conns, resource = c(
  opal_site = "PROJECT.cluster_a",
  armadillo_site = "hpcunits/resources/unit_alpha"
))

# dsImaging calls in this DataSHIELD session now use those selected units.

ds.hpc.unit.destroy(conns)
```

A scalar `DSConnection` is accepted too. If `resource` is omitted, the client
option `dshpc.unit.resource` is used; this option contains only a Resource name,
never a credential. Without an active selection, new jobs use the server's
pinned site default. Initialization is all-or-nothing across sites. If a
provider cannot remove a partial assignment, the error names only the affected
nodes; retry destruction and, if cleanup still fails, end those DataSHIELD
sessions before reconnecting.

## Domain-mediated submission pattern

Submission happens through a domain package. A shared submission handle
contains a public `tracking_id`; equivalent submissions may attach to that same
logical root without receiving another private capability. The tracking id is
the normal durable reference for status, approved results and output reuse.
Domains may also retain a capability for private compatibility or control
operations belonging to the original submission.

dsImaging exposes its domain workflow and tracking reference while keeping the
collection manifest, image jobs and assets behind its server-side contract:

```r
workflow <- dsImagingClient::ds.imaging.qc.metrics(
  conns, handle = "img", symbol = "qc_job"
)

dsImagingClient::ds.imaging.workflow.status(conns, workflow)
dsImagingClient::ds.imaging.workflow.destroy(conns, workflow)

# Once the domain exposes the root id, it is also discoverable in dsHPC Studio.
ds.hpc.studio(conns)
```

Only disclosure-safe output kinds explicitly marked by the domain package are
returned through these calls. Public `trk_...` ids address shared tracking and
approved knowledge. `ds.hpc.job_id()` remains available only for intentionally
private compatibility workflows that need a portable bearer; routine shared
monitoring does not use or return one. A raw internal execution-job identifier
is neither a tracking id nor an access credential.

## For Domain Package Developers

Domain packages should expose their own DataSHIELD methods and compose dsHPC
workflows server-side through `dsHPC::hpcSubmitInternal()`. The generic legacy
`hpcSubmitDS()` and `hpcLoadOutputDS()` entry points always fail, including on
deployments whose persisted allowlist still mentions them.

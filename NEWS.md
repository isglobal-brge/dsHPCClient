# dsHPCClient 0.4.0

* Restored `ds.hpc.list()`, `ds.hpc.summary()` and dsHPC Studio on the new
  versioned shared-root tracking API. Listing follows the full paginated
  history by default and Studio supports progressive paging and direct lookup
  by public `trk_...` id.
* `ds.hpc.status()`, `ds.hpc.result()`, `ds.hpc.outputs()` and
  `ds.hpc.wait()` now route public tracking ids to the disclosure-safe shared
  API while preserving the existing capability path for private symbols and
  bearers.
* Added `ds.hpc.load_output()` to assign an opaque reusable-output reference
  in the server R session. Artifact values, paths and underlying execution
  jobs never cross to the client.
* Studio now negotiates `root_v1`, `safe_v1` and `opaque_ref_v1` capabilities
  and degrades cleanly when shared tracking is unavailable. A selected reusable
  output can be assigned to an analyst-chosen server symbol without downloading
  it. Studio no longer contains scheduler, DAG, log, cancellation, timestamp,
  progress, retry, label or child-job panels.
* Client-side response normalization retains only the fixed public schemas:
  at most one `output_001` server object and one `output_002` count-only
  summary. It rejects raw domain names, arbitrary kinds, malformed values,
  oversized or missing pages, cursors, tracking status, and remote attributes
  with generic diagnostics.

# dsHPCClient 0.3.5

* Added `ds.hpc.unit.init()` and `ds.hpc.unit.destroy()` for selecting a
  server-managed execution unit through backend-neutral DSI Resource calls.
  Scalar connections and exact per-site Opal/Armadillo Resource names are
  supported without exposing credentials or remote transport errors.
* Unit initialization removes its temporary Resource and Armadillo's transient
  `R`/`rds` loader symbols on success and rollback, and verifies cleanup before
  returning.
* Incomplete multi-site rollback is now reported with only the affected node
  names and safe recovery guidance; provider messages, paths, and Resource
  names remain suppressed.

# dsHPCClient 0.3.4

* Named per-site bearer vectors returned by `ds.hpc.job_id()` are now routed to
  the matching node by status, result, output, log, wait, and admin-cancel
  helpers; a bearer from one site is never sent to another.
* Status and result requests retain successful node responses when another node
  fails, and transport warnings, messages, and errors are replaced with generic
  diagnostics so opaque bearers cannot enter client logs.
* Remote admin cancellation documents its asynchronous `REQUESTED` state and
  waits for server-side backend reconciliation before reporting completion.

# dsHPCClient 0.3.3

* Calls carrying job bearers or admin keys now suppress DataSHIELD expression
  progress and raw remote errors, restoring the caller's progress and error
  options even when a request fails.
* `ds.hpc.wait()` no longer prints opaque job bearers, preventing a console or
  notebook transcript from becoming a transferable job credential.
* `ds.hpc.job_id()` now uses the explicit `hpcJobReferenceDS()` endpoint.
  Routine status and result objects no longer contain transferable bearers.
* Retired analyst-wide job listing, summary, scheduler topology, and Studio
  entry points. Compatibility functions fail locally without making a
  DataSHIELD call, and the legacy summary renderer that printed job identifiers
  has been removed. Generic result printing redacts any bearer-shaped job
  reference defensively.
* Monitoring now documents and uses only an existing opaque domain-workflow
  symbol or per-job bearer. Public status no longer expects exact step,
  retry, label, or timestamp fields.
* Generic job submission and output loading examples were removed in favour
  of domain-mediated workflows such as dsImagingClient.

# dsHPCClient 0.3.2

* `print.dshpc_result()` now dispatches on payload shape: job listings render
  as a formatted table (previously states were concatenated, e.g.
  "FINISHEDFINISHEDFINISHED"), outputs render as name/kind/size rows, log
  payloads render as indented lines, and status payloads keep the one-line
  state format.
* `.ds_safe_aggregate()` no longer swallows per-server failures silently: one
  `warning()` is emitted per failed server at call time, and
  `print.dshpc_result()` renders the recorded `ds_errors`.
* New `ds.hpc.job_id(conns, symbol)` resolves the per-server job ids behind a
  `datashield.assign()` submission symbol (same server-side resolution as
  `ds.hpc.status()`); the symbol-based submission pattern is documented in the
  README, including the deduplication semantics (dedup applies against
  FINISHED/PUBLISHED jobs only; concurrent identical submissions both run).
* Documentation: `ds.hpc.scheduler_status()` return shape documented;
  runnable `\donttest` examples added to the key user-facing verbs
  (status/wait/result/list/logs/outputs/job_id).

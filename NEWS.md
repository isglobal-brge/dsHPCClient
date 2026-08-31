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

# Module: Admin Functions
# Disabled by default. Enabled when dshpc.admin_key is set on the server or
# DSHPC_ADMIN_KEY is set in the Rock/HPC environment.
# Key is B64-encoded for transport (Opal's R parser can't handle special chars).

#' List ALL jobs from ALL users (admin only)
#'
#' @param conns DSI connections object.
#' @param admin_key Character; the admin key matching `dshpc.admin_key` or
#'   `DSHPC_ADMIN_KEY` on the server.
#' @param label Character or NULL; filter by label.
#' @return A dshpc_result with per-site data.frames.
#' @export
ds.hpc.admin.list <- function(conns, admin_key, label = NULL) {
  key_enc <- .ds_encode(list(.admin_key = admin_key))
  results <- .ds_safe_aggregate(conns,
    expr = call("hpcAdminListDS", key_enc, label))
  dshpc_result(per_site = results)
}

#' Cancel any job (admin only)
#'
#' @param conns DSI connections object.
#' @param job_id Character; scalar job ID/bearer, or a named per-server vector.
#' @param admin_key Character; the admin key matching `dshpc.admin_key` or
#'   `DSHPC_ADMIN_KEY` on the server.
#' @return A `dshpc_result`. Remote jobs may report `cancellation = "REQUESTED"`
#'   and remain `RUNNING` until backend reconciliation completes.
#' @export
ds.hpc.admin.cancel <- function(conns, job_id, admin_key) {
  key_enc <- .ds_encode(list(.admin_key = admin_key))
  results <- .ds_safe_aggregate(conns,
    expr = function(srv) call("hpcAdminCancelDS",
      .ds_job_reference_for_site(job_id, srv), key_enc))
  dshpc_result(per_site = results)
}

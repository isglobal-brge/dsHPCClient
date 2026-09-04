# Module: backend-neutral HPC unit Resource selection

#' Select an HPC execution unit for the DataSHIELD session
#'
#' Assigns an Opal or Armadillo Resource through the DSI generic and replaces
#' it with an opaque, session-bound dsHPC handle. Later domain workflows in the
#' same server session are pinned to this unit when they submit durable jobs.
#'
#' `resource` defaults to the client-side option `dshpc.unit.resource`. A
#' scalar name is used at every site; a named vector/list must contain exactly
#' one Resource name for every connection. Resource names are opaque, so both
#' Opal's `PROJECT.resource` convention and Armadillo's
#' `project/folder/name` identifiers are accepted.
#'
#' @param conns A DSI connection or a named list of DSI connections.
#' @param resource Resource name, or an exact per-site named vector/list.
#' @param symbol Visible server symbol for the opaque unit handle.
#' @return `TRUE`, invisibly.
#' @export
ds.hpc.unit.init <- function(
    conns,
    resource = getOption("dshpc.unit.resource", NULL),
    symbol = "hpc_unit") {
  conns <- .hpc_unit_connections(conns)
  hosts <- .hpc_unit_hosts(conns)
  symbol <- .hpc_unit_symbol(symbol)
  resource_symbol <- .hpc_unit_resource_symbol(symbol)
  provider_transients <- c("R", "rds")
  route <- .hpc_unit_resource_route(resource, hosts)
  .hpc_unit_require_absent(conns,
    c(symbol, resource_symbol, provider_transients))
  .hpc_unit_prepare(conns)

  available <- vapply(hosts, function(host) {
    tryCatch(isTRUE(DSI::dsHasResource(conns[[host]], route[[host]])),
      error = function(e) FALSE)
  }, logical(1))
  if (!all(available)) {
    stop("HPC unit Resource is unavailable on: ",
      paste(hosts[!available], collapse = ", "), ".", call. = FALSE)
  }

  initialized <- FALSE
  init_attempted <- FALSE
  on.exit({
    rollback_failures <- character(0)
    if (init_attempted && !initialized) {
      report <- tryCatch(.hpc_unit_destroy_exact(conns, symbol),
        error = function(e) list(failures = paste0(hosts, ":rollback")))
      rollback_failures <- c(rollback_failures, report$failures)
    }
    for (temporary in c(provider_transients, resource_symbol)) {
      report <- tryCatch(.hpc_unit_remove_exact(conns, temporary),
        error = function(e) list(failures = paste0(hosts, ":rollback")))
      rollback_failures <- c(rollback_failures, report$failures)
    }
    if (!initialized && length(rollback_failures)) {
      nodes <- unique(sub(":(symbol-state|remove|destroy|rollback)$", "",
        rollback_failures))
      stop("HPC unit initialization failed and rollback was incomplete on: ",
        paste(nodes, collapse = ", "),
        ". Retry ds.hpc.unit.destroy() with the same symbol; if cleanup ",
        "still fails, end the affected DataSHIELD sessions.", call. = FALSE)
    }
  }, add = TRUE)

  .hpc_unit_assign_exact(conns, "HPC unit Resource assignment",
    function(success, error) {
      DSI::datashield.assign.resource(conns, symbol = resource_symbol,
        resource = as.list(route), success = success, error = error,
        errors.print = FALSE)
    })

  init_attempted <- TRUE
  .hpc_unit_assign_exact(conns, "HPC unit initialization",
    function(success, error) {
      DSI::datashield.assign.expr(conns, symbol = symbol,
        expr = call("hpcUnitInitDS", resource_symbol), success = success,
        error = error, errors.print = FALSE)
    })

  cleanup <- unlist(lapply(c(provider_transients, resource_symbol),
    function(temporary) .hpc_unit_remove_exact(conns, temporary)$failures),
    use.names = FALSE)
  if (length(cleanup)) {
    stop("Temporary HPC unit Resource cleanup failed on: ",
      paste(unique(cleanup), collapse = ", "), ".", call. = FALSE)
  }
  initialized <- TRUE
  invisible(TRUE)
}

#' Clear the selected HPC execution unit
#'
#' New jobs then use the server's site-wide default (embedded/local when the
#' administrator has not configured another default). Jobs already submitted
#' retain their sealed unit snapshot.
#'
#' @param conns A DSI connection or a named list of DSI connections.
#' @param symbol Unit handle symbol used by [ds.hpc.unit.init()].
#' @return `TRUE`, invisibly.
#' @export
ds.hpc.unit.destroy <- function(conns, symbol = "hpc_unit") {
  conns <- .hpc_unit_connections(conns)
  symbol <- .hpc_unit_symbol(symbol)
  report <- .hpc_unit_destroy_exact(conns, symbol)
  resource_report <- .hpc_unit_remove_exact(
    conns, .hpc_unit_resource_symbol(symbol))
  failures <- unique(c(report$failures, resource_report$failures))
  if (length(failures)) {
    stop("HPC unit destruction was incomplete on: ",
      paste(failures, collapse = ", "),
      ". Retry ds.hpc.unit.destroy() with the same symbol.", call. = FALSE)
  }
  invisible(TRUE)
}

.hpc_unit_connections <- function(conns) {
  if (inherits(conns, "DSConnection")) {
    host <- tryCatch(conns@name, error = function(e) NULL)
    if (!is.character(host) || length(host) != 1L || is.na(host) ||
        !nzchar(host)) {
      stop("A scalar DataSHIELD connection requires a non-empty node name.",
        call. = FALSE)
    }
    return(stats::setNames(list(conns), host))
  }
  conns
}

.hpc_unit_hosts <- function(conns) {
  hosts <- names(conns)
  if (!length(hosts) || anyNA(hosts) || any(!nzchar(hosts)) ||
      anyDuplicated(hosts)) {
    stop("DataSHIELD connections require non-empty, unique node names.",
      call. = FALSE)
  }
  hosts
}

.hpc_unit_symbol <- function(symbol) {
  if (!is.character(symbol) || length(symbol) != 1L || is.na(symbol) ||
      !grepl("^[A-Za-z][A-Za-z0-9._]{0,95}$", symbol)) {
    stop("A visible DataSHIELD symbol beginning with a letter is required.",
      call. = FALSE)
  }
  symbol
}

.hpc_unit_resource_symbol <- function(symbol) {
  paste0("dsHres.", substr(digest::digest(
    paste0("dsHPCClient:unit-resource:", .hpc_unit_symbol(symbol)),
    algo = "sha256", serialize = FALSE), 1L, 32L))
}

.hpc_unit_resource_route <- function(resource, hosts) {
  if (is.character(resource) && length(resource) == 1L &&
      is.null(names(resource))) {
    route <- stats::setNames(rep(resource, length(hosts)), hosts)
  } else {
    if (is.list(resource)) {
      valid <- vapply(resource, function(x) is.character(x) &&
        length(x) == 1L && !is.na(x), logical(1))
      if (!all(valid)) {
        stop("Per-site HPC unit Resources must be scalar names.", call. = FALSE)
      }
      route <- vapply(resource, identity, character(1))
    } else if (is.character(resource)) {
      route <- resource
    } else {
      stop("An HPC unit Resource name is required.", call. = FALSE)
    }
    route_names <- names(route)
    if (is.null(route_names) || anyNA(route_names) ||
        any(!nzchar(route_names)) || anyDuplicated(route_names) ||
        !setequal(route_names, hosts) || length(route) != length(hosts)) {
      stop("Per-site HPC unit Resources must name every connection exactly once.",
        call. = FALSE)
    }
    route <- route[hosts]
  }
  if (anyNA(route) || any(!nzchar(route)) ||
      any(nchar(route, type = "bytes") > 1024L) ||
      any(grepl("[\r\n]", route))) {
    stop("HPC unit Resource names must be non-empty scalars.", call. = FALSE)
  }
  route
}

.hpc_unit_transport <- function(expr) {
  old <- options(datashield.progress = FALSE,
    datashield.errors.print = FALSE, progress_enabled = FALSE)
  on.exit(options(old), add = TRUE)
  withCallingHandlers(force(expr),
    warning = function(w) invokeRestart("muffleWarning"),
    message = function(m) invokeRestart("muffleMessage"))
}

.hpc_unit_prepare <- function(conns) {
  hosts <- .hpc_unit_hosts(conns)
  unavailable <- character(0)
  for (host in hosts) {
    result <- tryCatch(
      .hpc_unit_transport(DSI::datashield.aggregate(
        conns[host], expr = quote(hpcCapabilitiesDS()))),
      error = function(e) NULL)
    capability <- if (is.list(result) && identical(names(result), host)) {
      result[[host]]
    } else NULL
    if (!is.list(capability) ||
        !identical(capability$status, "available") ||
        !identical(capability$execution_units,
          "resource_selection")) {
      unavailable <- c(unavailable, host)
    }
  }
  if (length(unavailable)) {
    stop("HPC unit support is unavailable on: ",
      paste(unavailable, collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

.hpc_unit_assign_exact <- function(conns, operation, invoke) {
  hosts <- .hpc_unit_hosts(conns)
  succeeded <- stats::setNames(rep(FALSE, length(hosts)), hosts)
  failed <- stats::setNames(rep(FALSE, length(hosts)), hosts)
  invalid <- FALSE
  success <- function(node, ...) {
    if (length(node) != 1L || is.na(node) || !node %in% hosts) invalid <<- TRUE
    else succeeded[[node]] <<- TRUE
  }
  error <- function(node, ...) {
    if (length(node) != 1L || is.na(node) || !node %in% hosts) invalid <<- TRUE
    else failed[[node]] <<- TRUE
  }
  thrown <- tryCatch({
    .hpc_unit_transport(invoke(success, error))
    NULL
  }, error = identity)
  bad <- hosts[!succeeded | failed]
  if (!is.null(thrown) || isTRUE(invalid) || length(bad)) {
    if (!length(bad)) bad <- hosts
    stop(operation, " failed or returned no ACK on: ",
      paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

.hpc_unit_node_symbols <- function(conns, host) {
  observed <- tryCatch(DSI::datashield.symbols(conns[host]), error = identity)
  if (inherits(observed, "error") || !is.list(observed) ||
      !identical(names(observed), host) || is.null(observed[[host]])) {
    return(list(ok = FALSE))
  }
  symbols <- as.character(observed[[host]])
  if (anyNA(symbols)) return(list(ok = FALSE))
  list(ok = TRUE, symbols = symbols)
}

.hpc_unit_require_absent <- function(conns, symbols) {
  hosts <- .hpc_unit_hosts(conns)
  symbols <- unique(vapply(symbols, .hpc_unit_symbol, character(1)))
  occupied <- character(0)
  for (host in hosts) {
    state <- .hpc_unit_node_symbols(conns, host)
    if (!isTRUE(state$ok)) {
      stop("Could not verify HPC unit symbol state on: ", host, ".",
        call. = FALSE)
    }
    if (any(symbols %in% state$symbols)) occupied <- c(occupied, host)
  }
  if (length(occupied)) {
    stop("HPC unit target symbol already exists on: ",
      paste(unique(occupied), collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

.hpc_unit_remove_exact <- function(conns, symbol) {
  symbol <- .hpc_unit_symbol(symbol)
  hosts <- .hpc_unit_hosts(conns)
  failures <- character(0)
  for (host in hosts) {
    before <- .hpc_unit_node_symbols(conns, host)
    if (!isTRUE(before$ok)) {
      failures <- c(failures, paste0(host, ":symbol-state"))
      next
    }
    if (!symbol %in% before$symbols) next
    error <- tryCatch({
      .hpc_unit_transport(DSI::datashield.rm(conns[host], symbol))
      NULL
    }, error = identity)
    after <- .hpc_unit_node_symbols(conns, host)
    if (!is.null(error) || !isTRUE(after$ok) || symbol %in% after$symbols) {
      failures <- c(failures, paste0(host, ":remove"))
    }
  }
  list(failures = unique(failures))
}

.hpc_unit_destroy_exact <- function(conns, symbol) {
  symbol <- .hpc_unit_symbol(symbol)
  hosts <- .hpc_unit_hosts(conns)
  failures <- character(0)
  for (host in hosts) {
    before <- .hpc_unit_node_symbols(conns, host)
    if (!isTRUE(before$ok)) {
      failures <- c(failures, paste0(host, ":symbol-state"))
      next
    }
    if (!symbol %in% before$symbols) next
    destroyed <- tryCatch({
      .hpc_unit_assign_exact(conns[host], "HPC unit destruction",
        function(success, error) {
          DSI::datashield.assign.expr(conns[host], symbol = symbol,
            expr = call("hpcUnitDestroyDS", symbol), success = success,
            error = error, errors.print = FALSE)
        })
      TRUE
    }, error = function(e) FALSE)
    if (!destroyed) {
      failures <- c(failures, paste0(host, ":destroy"))
      next
    }
    removed <- .hpc_unit_remove_exact(conns[host], symbol)
    if (length(removed$failures)) {
      failures <- c(failures, paste0(host, ":remove"))
    }
  }
  list(failures = unique(failures))
}

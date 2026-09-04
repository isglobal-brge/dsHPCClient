test_that("unit Resource routing is exact and backend-neutral", {
  hosts <- c("opal_site", "armadillo_site")
  expect_identical(
    dsHPCClient:::.hpc_unit_resource_route("shared-unit", hosts),
    c(opal_site = "shared-unit", armadillo_site = "shared-unit"))
  expect_identical(
    dsHPCClient:::.hpc_unit_resource_route(
      c(armadillo_site = "project/folder/arm-unit",
        opal_site = "PROJECT.opal-unit"),
      hosts),
    c(opal_site = "PROJECT.opal-unit",
      armadillo_site = "project/folder/arm-unit"))
  expect_identical(
    dsHPCClient:::.hpc_unit_resource_route(
      list(armadillo_site = "project/folder/arm-unit",
        opal_site = "opal-unit"), hosts),
    c(opal_site = "opal-unit",
      armadillo_site = "project/folder/arm-unit"))

  invalid <- list(
    c(opal_site = "only-one"),
    c(opal_site = "one", other = "two"),
    c("one", "two"),
    list(opal_site = c("one", "two"), armadillo_site = "arm"))
  for (route in invalid) {
    expect_error(dsHPCClient:::.hpc_unit_resource_route(route, hosts),
      "every connection exactly once|scalar names")
  }
})

.unit_fake_connections <- function() {
  list(
    opal_site = list(site = "opal_site"),
    armadillo_site = list(site = "armadillo_site"))
}

.unit_fake_capabilities <- function(conns, expr) {
  stats::setNames(lapply(names(conns), function(site) list(
    status = "available", execution_units = "resource_selection")),
    names(conns))
}

test_that("unit initialization uses only DSI generics and removes its Resource", {
  conns <- .unit_fake_connections()
  symbols <- list(opal_site = character(0), armadillo_site = character(0))
  assigned_resources <- character(0)
  assigned_methods <- character(0)

  testthat::local_mocked_bindings(
    datashield.aggregate = .unit_fake_capabilities,
    dsHasResource = function(conn, resource) {
      is.character(resource) && nzchar(resource)
    },
    datashield.symbols = function(conns) {
      lapply(names(conns), function(site) symbols[[site]]) |>
        stats::setNames(names(conns))
    },
    datashield.assign.resource = function(conns, symbol, resource,
                                           success = NULL, error = NULL, ...) {
      for (site in names(conns)) {
        assigned_resources[[site]] <<- resource[[site]]
        symbols[[site]] <<- union(symbols[[site]], symbol)
        if (identical(site, "armadillo_site")) {
          symbols[[site]] <<- union(symbols[[site]], c("R", "rds"))
        }
        success(site)
      }
      invisible(NULL)
    },
    datashield.assign.expr = function(conns, symbol, expr,
                                      success = NULL, error = NULL, ...) {
      method <- as.character(expr[[1L]])
      for (site in names(conns)) {
        assigned_methods <<- c(assigned_methods, method)
        symbols[[site]] <<- union(symbols[[site]], symbol)
        success(site)
      }
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol) {
      for (site in names(conns)) {
        symbols[[site]] <<- setdiff(symbols[[site]], symbol)
      }
      invisible(NULL)
    },
    .package = "DSI")

  expect_true(ds.hpc.unit.init(conns,
    resource = c(
      armadillo_site = "project/folder/armadillo-unit",
      opal_site = "PROJECT.opal-unit"),
    symbol = "hpc_unit"))
  expect_identical(assigned_resources,
    c(opal_site = "PROJECT.opal-unit",
      armadillo_site = "project/folder/armadillo-unit"))
  expect_identical(symbols,
    list(opal_site = "hpc_unit", armadillo_site = "hpc_unit"))
  expect_identical(assigned_methods,
    rep("hpcUnitInitDS", length(conns)))

  expect_true(ds.hpc.unit.destroy(conns, symbol = "hpc_unit"))
  expect_true(all(lengths(symbols) == 0L))
  expect_identical(tail(assigned_methods, length(conns)),
    rep("hpcUnitDestroyDS", length(conns)))
})

test_that("partial unit initialization rolls back without reflecting names", {
  conns <- .unit_fake_connections()
  symbols <- list(opal_site = character(0), armadillo_site = character(0))
  private_name <- "private/folder/unit"

  testthat::local_mocked_bindings(
    datashield.aggregate = .unit_fake_capabilities,
    dsHasResource = function(conn, resource) TRUE,
    datashield.symbols = function(conns) {
      lapply(names(conns), function(site) symbols[[site]]) |>
        stats::setNames(names(conns))
    },
    datashield.assign.resource = function(conns, symbol, resource,
                                           success = NULL, error = NULL, ...) {
      for (site in names(conns)) {
        symbols[[site]] <<- union(symbols[[site]],
          if (identical(site, "armadillo_site")) {
            c(symbol, "R", "rds")
          } else symbol)
        success(site)
      }
      invisible(NULL)
    },
    datashield.assign.expr = function(conns, symbol, expr,
                                      success = NULL, error = NULL, ...) {
      method <- as.character(expr[[1L]])
      for (site in names(conns)) {
        if (identical(method, "hpcUnitInitDS") &&
            identical(site, "armadillo_site")) {
          error(site, paste("remote detail for", private_name))
        } else {
          symbols[[site]] <<- union(symbols[[site]], symbol)
          success(site)
        }
      }
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol) {
      for (site in names(conns)) {
        symbols[[site]] <<- setdiff(symbols[[site]], symbol)
      }
      invisible(NULL)
    },
    .package = "DSI")

  message <- tryCatch({
    ds.hpc.unit.init(conns, resource = private_name, symbol = "hpc_unit")
    NA_character_
  }, error = conditionMessage)
  expect_match(message, "armadillo_site", fixed = TRUE)
  expect_false(grepl(private_name, message, fixed = TRUE))
  expect_true(all(lengths(symbols) == 0L))
})

test_that("unit initialization verifies availability before assignment", {
  conns <- .unit_fake_connections()
  assignment_called <- FALSE
  testthat::local_mocked_bindings(
    datashield.aggregate = .unit_fake_capabilities,
    dsHasResource = function(conn, resource) {
      !identical(conn$site, "armadillo_site")
    },
    datashield.symbols = function(conns) {
      lapply(names(conns), function(site) character(0)) |>
        stats::setNames(names(conns))
    },
    datashield.assign.resource = function(...) {
      assignment_called <<- TRUE
    },
    .package = "DSI")

  expect_error(ds.hpc.unit.init(conns, resource = "unit"),
    "armadillo_site", fixed = TRUE)
  expect_false(assignment_called)
})

test_that("a scalar DSConnection uses only backend-neutral DSI calls", {
  if (!methods::isClass("DsHpcClientUnitTestConnection")) {
    methods::setClass("DsHpcClientUnitTestConnection", contains = "DSConnection")
  }
  conn <- methods::new("DsHpcClientUnitTestConnection",
    name = "armadillo_site")
  symbols <- character(0)
  assigned_connection <- NULL

  testthat::local_mocked_bindings(
    datashield.aggregate = .unit_fake_capabilities,
    dsHasResource = function(conn, resource) {
      inherits(conn, "DSConnection") &&
        identical(resource, "project/folder/unit")
    },
    datashield.symbols = function(conns) {
      stats::setNames(list(symbols), names(conns))
    },
    datashield.assign.resource = function(conns, symbol, resource,
                                           success = NULL, error = NULL, ...) {
      assigned_connection <<- conns[[1L]]
      symbols <<- union(symbols, symbol)
      success(names(conns)[[1L]])
      invisible(NULL)
    },
    datashield.assign.expr = function(conns, symbol, expr,
                                      success = NULL, error = NULL, ...) {
      symbols <<- union(symbols, symbol)
      success(names(conns)[[1L]])
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol) {
      symbols <<- setdiff(symbols, symbol)
      invisible(NULL)
    },
    .package = "DSI")
  testthat::local_mocked_bindings(
    .detect_backend = function(...) {
      stop("backend detection must not be called")
    },
    .package = "dsHPCClient")

  expect_true(ds.hpc.unit.init(conn, resource = "project/folder/unit"))
  expect_identical(assigned_connection, conn)
  expect_identical(symbols, "hpc_unit")
  expect_true(ds.hpc.unit.destroy(conn))
  expect_length(symbols, 0L)
})

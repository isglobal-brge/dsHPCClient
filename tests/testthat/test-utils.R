test_that("ds_encode produces B64 prefix for lists", {
  encoded <- dsHPCClient:::.ds_encode(list(a = 1, b = "hello"))
  expect_true(startsWith(encoded, "B64:"))
})

test_that("ds_encode is identity for scalars", {
  expect_equal(dsHPCClient:::.ds_encode("hello"), "hello")
  expect_equal(dsHPCClient:::.ds_encode(42), 42)
})

test_that("ds_encode handles vectors", {
  encoded <- dsHPCClient:::.ds_encode(c("a", "b", "c"))
  expect_true(startsWith(encoded, "B64:"))
})

test_that("generate_symbol has correct format", {
  sym <- dsHPCClient:::.generate_symbol("dsH")
  expect_true(startsWith(sym, "dsH."))
  expect_equal(nchar(sym), 10L)
})

test_that("generate_symbol is unique", {
  syms <- replicate(100, dsHPCClient:::.generate_symbol())
  expect_equal(length(unique(syms)), 100L)
})

test_that(".ds_safe_aggregate warns per failed server and records ds_errors", {
  fake_conns <- list(siteX = structure(list(), class = "not_a_connection"))
  expect_warning(
    res <- dsHPCClient:::.ds_safe_aggregate(fake_conns, quote(anyCallDS())),
    "siteX")
  errs <- attr(res, "ds_errors")
  expect_false(is.null(errs))
  expect_true("siteX" %in% names(errs))
})

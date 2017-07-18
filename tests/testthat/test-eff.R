context("eff")

test_that("eff() can make unary functions without default values", {
  expect_equal(eff(x ~ x + 1), function(x) x + 1)
})

test_that("eff() can make unary functions with default values", {
  expect_equal(eff(x = 1 ~ x + 1), function(x = 1) x + 1)
})

test_that("eff() can make variadic functions", {
  expect_equal(eff(... ~ list(...)), function(...) list(...))
})

test_that("eff() can make multi-argument functions without default values", {
  expect_equal(eff(x, y ~ x + y), function(x, y) x + y)
})

test_that("eff() can make multi-argument functions with default values", {
  expect_equal(
    eff(x, y = 1, z = x ~ x + y + z),
    function(x, y = 1, z = x) x + y + z
  )
})

test_that("eff() can make multi-argument, variadic functions", {
  expect_equal(eff(x, y = 1, ... ~ x + y), function(x, y = 1, ...) x + y)
  expect_equal(eff(x, ... = , y = 1 ~ x + y), function(x, ..., y = 1) x + y)
})

test_that("error signaled if eff() is called without any arguments", {
  expect_error(eff(), "No function body specified")
})

test_that("eff() with no LHS creates a function with empty signature", {
  expect_equal(eff(~NULL), function() NULL)
})
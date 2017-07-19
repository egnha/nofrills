context("Creating functions")

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
  expect_equal(eff(x, y = 0 ~ x + y), function(x, y = 0) x + y)
  expect_equal(eff(x, y = x ~ x + y), function(x, y = x) x + y)
  expect_equal(eff(x = 0, y ~ x + y), function(x = 0, y) x + y)
  expect_equal(eff(x = y, y ~ x + y), function(x = y, y) x + y)
  expect_equal(eff(x = 0, y = 1 ~ x + y), function(x = 0, y = 1) x + y)
})

test_that("eff() can make multi-argument, variadic functions", {
  expect_equal(eff(x, y, ... ~ x + y), function(x, y, ...) x + y)
  expect_equal(eff(x, ... = , y ~ x + y), function(x, ..., y) x + y)
  expect_equal(eff(x, y = 1, ... ~ x + y), function(x, y = 1, ...) x + y)
  expect_equal(eff(x, ... = , y = 1 ~ x + y), function(x, ..., y = 1) x + y)
})

test_that("eff() with no LHS creates a function with empty signature", {
  expect_equal(eff(~NULL), function() NULL)
})

context("Function environment")

test_that("eff() creates a function in the calling environment, by default", {
  env <- environment()
  f <- eff(x ~ NULL)
  expect_identical(environment(f), env)
})

test_that("eff() creates a function whose environment is ..env", {
  env <- new.env()
  f <- eff(x ~ NULL, ..env = env)
  expect_identical(environment(f), env)
})

context("Error handling")

test_that("error signaled if eff() is called without any arguments", {
  expect_error(eff(), "No function body specified")
})

test_that("error signaled if final argument is not a formula", {
  expect_error(eff(x), "Final argument must be a formula")
  expect_error(eff(x, y), "Final argument must be a formula")
})

test_that("error signaled if final argument has = but no default value", {
  expect_error(eff(x = ~ NULL), "Default value of final argument is missing")
  expect_error(eff(x, y = ~ NULL), "Default value of final argument is missing")
})

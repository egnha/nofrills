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

test_that("eff() accepts a separating comma, between the arguments and body", {
  expect_equal(eff(x, ~ NULL), function(x) NULL)
  expect_equal(eff(... = , ~ NULL), function(...) NULL)
  expect_equal(eff(x = 1, ~ NULL), function(x = 1) NULL)
})

test_that("you can find the answer to the riddle", {
  expect_error(..(8~D)(), NA)
  expect_error(..(~8^D)())
})

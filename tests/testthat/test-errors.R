context("Error handling")

context("fn()")

test_that("error signaled if fn() is called without any arguments", {
  expect_error(fn(), "No function specified")
})

test_that("error signaled if any formuals appear before the body-formula", {
  msg <- "Only the body \\(as last argument\\) should be a formula"
  expect_error(fn(x ~ NULL, y), msg)
  expect_error(fn(a, x ~ NULL, b), msg)
  expect_error(fn(a ~ b, x ~ NULL), msg)
})

test_that("error signaled if final argument is not a formula", {
  msg <- "Final argument must be a formula \\(specifying the body\\)"
  expect_error(fn(x), msg)
  expect_error(fn(x, y), msg)
})

test_that("error signaled if final argument has = but no default value", {
  msg <- "Default value of final argument is missing"
  expect_error(fn(x = ~ NULL), msg)
  expect_error(fn(x, y = ~ NULL), msg)
})

test_that("error signaled if '..env' is not an environment", {
  expect_error(fn(~ NULL, ..env = NULL), "'..env' must be an environment")
})

context("as_fn()")

foo <- function(x) as_fn(x)

test_that("error signaled if '.f' is neither function nor anonymous function", {
  expect_error(foo(NULL), "mode 'function' was not found")
})

test_that("error signaled if as_fn() applied across calls", {
  bar <- function(y) foo(y)
  expect_error(bar(.(x ~ x + 1)), 'could not find function "."')
})

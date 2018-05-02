context("Error handling")

context("fn(), fn_()")

test_that("error signaled if fn(), fn_() are called without arguments", {
  expect_error(fn(), "Function must be declared")
  expect_error(fn_(), "Function must be declared")
})

test_that("error signaled if any formuals appear before the body-formula", {
  expect_errors_with_message(
    "Only the body \\(as last argument\\) should be a formula",
    fn(x ~ NULL, y),
    fn(a, x ~ NULL, b),
    fn(a ~ b, x ~ NULL),
    fn_(x ~ NULL, y),
    fn_(a, x ~ NULL, b),
    fn_(a ~ b, x ~ NULL)
  )
})

test_that("error signaled if final argument is not a formula", {
  expect_errors_with_message(
    "Final argument must be a formula \\(specifying the body\\)",
    fn(x),
    fn(x, y),
    fn_(x),
    fn_(x, y)
  )
})

test_that("error signaled if final argument has = but no default value", {
  expect_errors_with_message(
    "Default value of final argument expected",
    fn(x = ~ NULL),
    fn(x, y = ~ NULL),
    fn_(x = ~ NULL),
    fn_(x, y = ~ NULL)
  )
})

test_that("error signaled if '..env' is not an environment", {
  expect_error(fn(~ NULL, ..env = NULL), "'..env' must be an environment")
  expect_error(fn_(~ NULL, ..env = NULL), "'..env' must be an environment")
})

context("as_fn()")

foo <- function(x) suppressWarnings(as_fn(x))

test_that("error signaled if '.f' is neither function nor anonymous function", {
  expect_error(foo(NULL), "mode 'function' was not found")
})

test_that("error signaled if as_fn() applied across calls", {
  bar <- function(y) foo(y)
  expect_error(bar(.(x ~ x + 1)), 'could not find function "."')
})

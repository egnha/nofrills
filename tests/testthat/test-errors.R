context("Error handling")

test_that("error signaled if eff() is called without any arguments", {
  expect_error(eff(), "No function specified")
})

test_that("error signaled if any formuals appear before the body-formula", {
  msg <- "Only the body \\(as last argument\\) should be a formula"
  expect_error(eff(x ~ NULL, y), msg)
  expect_error(eff(a, x ~ NULL, b), msg)
  expect_error(eff(a ~ b, x ~ NULL), msg)
})

test_that("error signaled if final argument is not a formula", {
  msg <- "Final argument must be a formula \\(specifying the body\\)"
  expect_error(eff(x), msg)
  expect_error(eff(x, y), msg)
})

test_that("error signaled if final argument has = but no default value", {
  msg <- "Default value of final argument is missing"
  expect_error(eff(x = ~ NULL), msg)
  expect_error(eff(x, y = ~ NULL), msg)
})

test_that("error signaled if '..env' is not an environment", {
  expect_error(eff(~ NULL, ..env = NULL), "'..env' must be an environment")
})

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

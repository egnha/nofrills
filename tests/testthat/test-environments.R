context("Function environment")

context("eff()")

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

context("as_eff()")

test_that("as_eff() creates a function in the caller's calling environment", {
  foo <- function(x) as_eff(x)
  env <- environment()
  f <- foo(.(x ~ NULL))
  expect_identical(environment(f), env)
})

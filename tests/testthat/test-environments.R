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

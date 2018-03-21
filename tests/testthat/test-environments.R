context("Function environment")

context("fn()")

test_that("fn() creates a function in the calling environment, by default", {
  env <- environment()
  f <- fn(x ~ NULL)
  expect_identical(environment(f), env)
})

test_that("fn() creates a function whose environment is ..env", {
  env <- new.env()
  f <- fn(x ~ NULL, ..env = env)
  expect_identical(environment(f), env)
})

context("as_fn()")

test_that("as_fn() creates a function in the caller's calling environment", {
  foo <- function(x) as_fn(x)
  env <- environment()
  f <- suppressWarnings(foo(.(x ~ NULL)))
  expect_identical(environment(f), env)
})

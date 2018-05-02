context("Function environment")

context("fn(), fn_()")

test_that("fn(), fn_() create functions in the calling environment, by default", {
  env <- environment()

  f <- fn(x ~ NULL)
  expect_identical(environment(f), env)

  f <- fn_(x ~ NULL)
  expect_identical(environment(f), env)
})

test_that("fn(), fn_() create functions whose environment is ..env", {
  env <- new.env()

  f <- fn(x ~ NULL, ..env = env)
  expect_identical(environment(f), env)

  f <- fn_(x ~ NULL, ..env = env)
  expect_identical(environment(f), env)
})

context("as_fn()")

test_that("as_fn() creates a function in the caller's calling environment", {
  foo <- function(x) as_fn(x)
  env <- environment()
  f <- suppressWarnings(foo(.(x ~ NULL)))
  expect_identical(environment(f), env)
})

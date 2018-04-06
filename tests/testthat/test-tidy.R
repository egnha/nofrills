context("Tidy functions")

test_that("ordinary function calls are made as usual", {
  f <- function(x, y = x, ..., z = "z") list(x, y, z, ...)
  f_tidy <- tidy(f)

  expect_identical(f_tidy(1), f(1))
  expect_identical(f_tidy(1, 2), f(1, 2))
  expect_identical(f_tidy(1, 2, 3), f(1, 2, 3))
  expect_identical(f_tidy(1, z = 0), f(1, z = 0))
})

test_that("arguments can be unquoted", {
  f <- function(x, ...) c(x, ...)
  f_tidy <- tidy(f)

  x <- "value"
  xq <- local({
    val <- "quosured value"
    rlang::quo(val)
  })

  expect_identical(f_tidy(!!x), "value")
  expect_identical(f_tidy("a", !!x), c("a", "value"))
  expect_identical(f_tidy(!!xq), "quosured value")
})

test_that("arguments can be spliced", {
  f <- function(x, y, ..., z = "z") c(x, y, z, ...)
  f_tidy <- tidy(f)

  expect_identical(f_tidy("x", !!! list("y")), f("x", "y"))
  expect_identical(f_tidy("x", !!! list("y", "w")), f("x", "y", "w"))
  expect_identical(f_tidy(!!! list(y = "y", "x")), c("x", "y", "z"))
})

test_that("tidying is idempotent", {
  f <- function(x, y = x, ..., z = "z") list(x, y, z, ...)
  f_t <- tidy(f)
  f_tt <- tidy(tidy(f))

  expect_equal(f_tt, f_t)

  expect_identical(f_tt(1), f_t(1))
  expect_identical(f_tt(1, 2), f_t(1, 2))
  expect_identical(f_tt(1, 2, 3), f_t(1, 2, 3))
  expect_identical(f_tt(1, z = 0), f_t(1, z = 0))
})

test_that("only functions can be tidy", {
  fs <- list(c, identity, function() NULL)
  for (f in fs)
    expect_true(is_tidy(tidy(f)))

  non_fs <- list(NULL, "c", ~c(), quote(c), quote(identity))
  for (x in non_fs)
    expect_false(is_tidy(x))
})

test_that("functions with void formals are vacuously tidy", {
  void_fs <- list(function() NULL, closure = Sys.time, primitive = globalenv)
  for (f in void_fs)
    expect_identical(tidy(f), f)
})

test_that("untidying undoes tidying", {
  f <- function(x, y = x, ..., z = "z") list(x, y, z, ...)
  f_ <- untidy(tidy(f))

  expect_equal(f_, f)
  expect_false(is_tidy(f_))

  expect_identical(f_(1), f_(1))
  expect_identical(f_(1, 2), f_(1, 2))
  expect_identical(f_(1, 2, 3), f_(1, 2, 3))
  expect_identical(f_(1, z = 0), f_(1, z = 0))
})

test_that("error is signaled when attempting to tidy a non-function", {
  non_fs <- list(NULL, "c", ~c(), quote(c), quote(identity))
  for (x in non_fs)
    expect_error(tidy(x), "Only functions can be tidied")
})

test_that("error is signaled when attempting to untidy a non-function", {
  non_fs <- list(NULL, "c", ~c(), quote(c), quote(identity))
  for (x in non_fs)
    expect_error(untidy(x), "Only functions can be untidied")
})

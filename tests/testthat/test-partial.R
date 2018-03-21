context("Partial function application")

test_that("function returned unchanged when no arguments to fix", {
  f <- function(x, y) NULL
  expect_identical(partial(f), f)
})

test_that("arguments can be fixed", {
  f <- function(x, y) c(x, y)

  fp <- partial(f, x = 0)
  expect_identical(fp(1), c(0, 1))
  expect_identical(fp(y = 1), c(0, 1))

  fp <- partial(f, y = 1)
  expect_identical(fp(0), c(0, 1))
  expect_identical(fp(x = 0), c(0, 1))

  fp <- partial(f, x = 0, y = 1)
  expect_identical(fp(), c(0, 1))
  expect_error(fp(2), "unused argument")
})

test_that("arguments can be lazily fixed when ..lazy = TRUE (default)", {
  f <- function(x, y, z) c(x, y)

  fp <- partial(f, z = stop("!"))
  expect_identical(fp(0, 1), c(0, 1))

  fp <- partial(f, x = x, ..lazy = TRUE)
  x <- 0
  expect_identical(fp(1), c(0, 1))
  x <- 1
  expect_identical(fp(1), c(1, 1))

  fp <- partial(f, x = x)
  x <- 0
  expect_identical(fp(1), c(0, 1))
  x <- 1
  expect_identical(fp(1), c(1, 1))
})

test_that("arguments can be eagerly fixed when ..lazy = FALSE", {
  x <- 0
  value <- c(0, 1)
  f <- function(x, y) c(x, y)
  fp <- partial(f, x = x, ..lazy = FALSE)
  expect_identical(fp(1), value)
  x <- 1
  expect_identical(fp(1), value)

  expect_error(partial(f, x = stop("!"), ..lazy = FALSE), "!")
})

test_that("quosure arguments can be tidily fixed when ..lazy = FALSE", {
  x <- local({x <- 0; rlang::quo(x)})
  f <- function(x, y) c(x, y)
  fp <- partial(f, x = !! x, ..lazy = FALSE)
  expect_identical(fp(1), c(0, 1))
})

test_that("arguments to fix can be unquoted", {
  f <- function(x, y) c(x, y)
  x <- 0
  value <- c(0, 1)
  fp <- partial(f, x = !! x)
  expect_identical(fp(1), value)
  x <- 1
  expect_identical(fp(1), value)
})

test_that("arguments to fix can be spliced", {
  value <- c(0, 1)
  f <- function(x, y) c(x, y)
  fp <- partial(f, !!! list(x = 0))
  expect_identical(fp(1), value)
  fp <- partial(f, !!! list(y = 1))
  expect_identical(fp(0), value)
  fp <- partial(f, !!! list(x = 0, y = 1))
  expect_identical(fp(), value)
})

test_that("arguments to fix are evaluated in environment ..env", {
  x <- "global"  # Doppelgaenger
  env <- local({x <- "local"; environment()})
  f <- function(x, y) x
  fp <- partial(f, x = x, ..env = env)
  expect_identical(fp(), "local")
})

test_that("by default, arguments to fix are evaluated in calling environment", {
  x <- "global"  # Doppelgaenger
  env <- local({x <- "local"; environment()})
  f <- function(x, y) x
  fp <- evalq(partial(f, x = x), env)
  expect_identical(fp(), "local")
})

test_that("error is signaled when value to fix doesn't match an argument", {
  f <- function(x, y) NULL
  expect_error(partial(f, 0), "Values to fix must be named by arguments of 'f'")
  expect_error(partial(f, xx = 0), "Values to fix must be named by arguments of 'f'")
  expect_error(partial(f), NA)
  expect_error(partial(f, x = 0), NA)
  expect_error(partial(f, x = 0, y = 1), NA)
})

test_that("error is signaled when trying to call a fixed value", {
  fp <- partial(function(x, y) NULL, x = 0)
  expect_error(fp(x = 1), "unused argument \\(x = 1\\)")
})

test_that("formals are contracted", {
  f <- function(x, y = x, ..., z = 0) NULL
  expect_equal(
    formals(partial(f)),
    formals(f)
  )
  expect_equal(
    formals(partial(f, x = 1)),
    formals(function(y = 1, ..., z = 0) {})
  )
  expect_equal(
    formals(partial(f, x = one)),
    formals(function(y = one, ..., z = 0) {})
  )
  expect_equal(
    formals(partial(f, y = 2)),
    formals(function(x, ..., z = 0) {})
  )
  expect_equal(
    formals(partial(f, z = 3)),
    formals(function(x, y = x, ...) {})
  )
  expect_equal(
    formals(partial(f, x = 1, y = 2)),
    formals(function(..., z = 0) {})
  )
  expect_equal(
    formals(partial(f, x = 1, z = 3)),
    formals(function(y = 1, ...) {})
  )
  expect_equal(
    formals(partial(f, y = 2, z = 3)),
    formals(function(x, ...) {})
  )
  expect_equal(
    formals(partial(f, x = 1, y = 2, z = 3)),
    formals(function(...) {})
  )
})

test_that("partial() is compatible with functions using missing()", {
  f <- function(x) {
    if (missing(x))
      x <- "x is missing"
    x
  }
  fp <- partial(f, x = "x is fixed")
  expect_equal(formals(fp), formals(function() {}))
  expect_equal(fp(), "x is fixed")
  expect_equal(f(), "x is missing")
})

context("Inverting partial function application")

test_that("de-partialzing a partial function recovers the original function", {
  f <- function(x, y) NULL
  fp <- partial(f, x = 1)
  expect_identical(departial(fp), f)
})

test_that("de-partializing a departialized function returns the function", {
  f <- function(x, y) NULL
  expect_identical(departial(f), f)
})

test_that("error is signaled when attempting to de-partialize a non-function", {
  expect_errors_with_message(
    "Only functions can be de-partialized",
    departial(NULL),
    departial(quote(function(x) NULL))
  )
})

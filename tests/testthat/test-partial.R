fn_kinds <- list(
  closure = identity,
  special = log,
  builtin = c,
  partial = partial(function(x, y) c(x, y), x = 0)
)

context("Partial function application")

test_that("function returned unchanged when no argument values to fix", {
  for (f in fn_kinds)
    expect_identical(partial(f), f)
})

test_that("named argument values can be fixed", {
  f <- function(x, y) c(x, y)
  out <- c(0, 1)

  fix_x <- partial(f, x = 0)
  expect_identical(fix_x(1), fix_x(y = 1))
  expect_identical(fix_x(1), out)

  fix_y <- partial(f, y = 1)
  expect_identical(fix_y(0), fix_y(x = 0))
  expect_identical(fix_y(0), out)

  fix_xy <- partial(f, x = 0, y = 1)
  expect_identical(fix_xy(), out)
  expect_error(fix_xy(dummy), "unused argument")
})

test_that("named dots-arguments can be fixed", {
  f <- function(x, ..., y = 3) c(x, ..., y)

  out <- c(1, a = 2, 3)
  expect_equal(out, partial(f, a = 2)(1))
  expect_equal(out, partial(f, 1, a = 2)())
  expect_equal(out, partial(f, a = 2, 1)())

  out <- c(1, a = 2, b = 2.5, 3)
  expect_equal(out, partial(f, a = 2, b = 2.5)(1))
  expect_equal(out, partial(f, a = 2, b = 2.5, 1)())
  expect_equal(out, partial(partial(f, a = 2), b = 2.5)(1))
  expect_equal(out, partial(partial(partial(f, 1), a = 2), b = 2.5)())
  expect_equal(out, partial(partial(partial(f, a = 2), 1), b = 2.5)())
  expect_equal(out, partial(partial(partial(f, a = 2), b = 2.5), 1)())

  out <- c(1, a = 2, b = 3, 4)
  expect_equal(out, partial(f, a = 2, b = 3, y = 4)(1))
  expect_equal(out, partial(partial(f, a = 2, b = 3), y = 4)(1))
  expect_equal(out, partial(partial(f, a = 2, y = 4), b = 3)(1))
  expect_equal(out, partial(partial(f, y = 4, a = 2), b = 3)(1))
  expect_equal(out, partial(partial(f, a = 2), b = 3, y = 4)(1))
  expect_equal(out, partial(partial(f, y = 4), a = 2, b = 3)(1))
  expect_equal(out, partial(partial(partial(f, y = 4), a = 2), b = 3)(1))
  expect_equal(out, partial(partial(partial(f, a = 2), y = 4), b = 3)(1))
  expect_equal(out, partial(partial(partial(f, a = 2), b = 3), y = 4)(1))
})

test_that("unnamed dots-arguments can be fixed", {
  f <- function(x, ...) c(x, ...)

  out <- c(1, 2)
  expect_equal(out, partial(f, 1, 2)())
  expect_equal(out, partial(f, 2, x = 1)())
  expect_equal(out, partial(f, x = 1, 2)())

  out <- c(1, 2, 3)
  expect_equal(out, partial(f, 1, 2, 3)())
  expect_equal(out, partial(f, x = 1, 2, 3)())
  expect_equal(out, partial(f, 2, x = 1, 3)())
  expect_equal(out, partial(f, 2, 3, x = 1)())
  expect_equal(out, partial(partial(f, 1), 2, 3)())
  expect_equal(out, partial(partial(f, x = 1), 2, 3)())
  expect_equal(out, partial(partial(f, 1, 2), 3)())
  expect_equal(out, partial(partial(f, x = 1, 2), 3)())
  expect_equal(out, partial(partial(f, 2, x = 1), 3)())
  expect_equal(out, partial(partial(partial(f, 1), 2), 3)())
  expect_equal(out, partial(partial(partial(f, x = 1), 2), 3)())
})

test_that("dots persist", {
  expect_dots <- function(...) {
    fs <- eval(substitute(alist(...)))
    for (f in fs) {
      expectation <- bquote(expect_true("..." %in% names(formals(.(f)))))
      eval.parent(expectation)
    }
  }

  f <- function(x, y, ..., z = 3) c(x, y, ..., z)
  expect_dots(
    partial(f),
    partial(f, 1),
    partial(f, y = 2),
    partial(f, z = 3),
    partial(f, 1, 2),
    partial(f, 1, z = 3),
    partial(f, y = 2, z = 3),
    partial(f, 1, 2, 3),
    partial(f, 1, 2, a = 3),
    partial(f, 1, 2, 3, 4),
    partial(f, 1, 2, 3, a = 4),
    partial(f, 1, 2, a = 3, 4),
    partial(partial(f, 1), 2),
    partial(partial(f, 1), 2, 3),
    partial(partial(f, 1), 2, a = 3),
    partial(partial(f, 1, 2), 3),
    partial(partial(f, 1, 2), a = 3),
    partial(partial(partial(f, 1), 2), 3),
    partial(partial(partial(f, 1), 2), a = 3),
    partial(f, 1, 2, 3, z = 5),
    partial(f, 1, 2, 3, 4, z = 5),
    partial(f, 1, 2, 3, a = 4, z = 5),
    partial(f, 1, 2, a = 3, 4, z = 5),
    partial(f, 1, 2, a = 3, b = 4, z = 5)
  )
})

test_that("partial() is operationally idempotent", {
  f <- function(x, y, ..., z = 3) c(x, y, ..., z)

  expect_equal(
    partial(partial(f), 1),
    partial(f, 1)
  )
  expect_equal(
    departial(partial(partial(f), 1)),
    departial(partial(f, 1))
  )
  expect_equal(partial(partial(f), 1)(2), partial(f, 1)(2))
  expect_equal(partial(partial(f), 1)(2), c(1, 2, 3))

  expect_equal(
    partial(partial(f, 1), 2),
    partial(f, 1, 2)
  )
  expect_equal(
    departial(partial(partial(f, 1), 2)),
    departial(partial(f, 1, 2))
  )
  expect_equal(partial(partial(f, 1), 2)(), partial(f, 1, 2)())
  expect_equal(partial(partial(f, 1), 2)(), c(1, 2, 3))

  expect_equal(
    partial(partial(partial(f, 1), 2), 2.5),
    partial(f, 1, 2, 2.5)
  )
  expect_equal(
    departial(partial(partial(partial(f, 1), 2), 2.5)),
    departial(partial(f, 1, 2, 2.5))
  )
  expect_equal(
    partial(partial(partial(f, 1), 2), 2.5)(),
    partial(f, 1, 2, 2.5)()
  )
  expect_equal(
    partial(partial(partial(f, 1), 2), 2.5)(),
    c(1, 2, 2.5, 3)
  )

  expect_equal(
    partial(partial(partial(partial(f, 1), 2), 3), z = 4),
    partial(f, 1, 2, 3, z = 4)
  )
  expect_equal(
    departial(partial(partial(partial(partial(f, 1), 2), 3), z = 4)),
    departial(partial(f, 1, 2, 3, z = 4))
  )
  expect_equal(
    partial(partial(partial(partial(f, 1), 2), 3), z = 4)(),
    partial(f, 1, 2, 3, z = 4)()
  )
  expect_equal(
    partial(partial(partial(partial(f, 1), 2), 3), z = 4)(),
    c(1, 2, 3, 4)
  )
})

test_that("argument values are captured lazily (by default)", {
  expect_error(partial(identity, x = stop("!")), NA)
  expect_error(partial(identity, x = stop("!"))(), "!")
})

test_that("argument values are captured eagerly with unquoting", {
  set_value <- function() {
    is_value_set <<- TRUE
    runif(1)
  }
  is_value_set <- FALSE

  partial(identity, x = set_value())
  expect_identical(is_value_set, FALSE)

  f <- partial(identity, x = !! set_value())
  expect_identical(is_value_set, TRUE)

  out <- f()
  expect_identical(f(), out)
})

test_that("argument values are tidily evaluated", {
  env <- local({
    value <- "x"
    environment()
  })
  value <- local({
    value_ <- "y"
    quo(value_)
  })

  f <- function(x, y) c(x, y)
  fp <- evalq(partial(f, x = value), env)
  fpp <- partial(fp, y = !! value)

  expect_identical(fpp(), c("x", "y"))
})

test_that("argument values can be spliced", {
  f <- function(x, y) c(x, y)
  out <- c(0, 1)

  fp <- partial(f, !!! list(x = 0))
  expect_identical(fp(1), out)

  fp <- partial(f, !!! list(y = 1))
  expect_identical(fp(0), out)

  fp <- partial(f, !!! list(x = 0, y = 1))
  expect_identical(fp(), out)
})

test_that("error is signaled when value to fix doesn't match an argument", {
  f <- function(x, y) NULL
  expect_errors_with_message(
    "unused argument",
    partial(f, 0, 1, 2),
    partial(f, z = 0)
  )
  expect_errors_with_message(
    NA,
    partial(f),
    partial(f, x = 0),
    partial(f, x = 0, y = 1)
  )
})

test_that("error is signaled when trying to fix a previously fixed argument", {
  f <- function(x, y, ...) NULL
  expect_errors_with_message(
    "Can't reset previously fixed argument\\(s\\)",
    partial(partial(f, x = 1), x = 1),
    partial(partial(f, 1), x = 1),
    partial(partial(f, 1, a = 2), a = 2),
    partial(partial(f, x = 1, y = 2), y = 2)
  )
})

test_that("error is signaled when trying to call a fixed argument", {
  expect_error(partial(identity, x = 0)(x = 1), "unused argument \\(x = 1\\)")
})

test_that("formals are literally truncated", {
  f <- function(x, y = x, ..., z = 0) NULL
  expect_equal(
    formals(partial(f)),
    formals(f)
  )
  expect_equal(
    formals(partial(f, x = 1)),
    formals(function(y = x, ..., z = 0) {})
  )
  expect_equal(
    formals(partial(f, x = one)),
    formals(function(y = x, ..., z = 0) {})
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
    formals(function(y = x, ...) {})
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

test_that("fixed arguments are not missing", {
  f <- function(x) {
    if (missing(x))
      x <- "x is missing"
    x
  }
  fix_x <- partial(f, x = "x is fixed")
  expect_equal(formals(fix_x), formals(function() {}))
  expect_identical(fix_x(), "x is fixed")
  expect_identical(f(), "x is missing")
})

context("Inverting partial function application")

test_that("departial() for a partial function recovers the original function", {
  f <- function(x, y) c(x, y)
  fp <- partial(f, x = 0)
  fpp <- partial(fp, y = 1)
  expect_identical(departial(fp), f)
  expect_identical(departial(fpp), f)
})

test_that("departial() is the identity for non-partial functions", {
  not_partial <- names(fn_kinds) != "partial"
  for (f in fn_kinds[not_partial])
    expect_identical(departial(f), f)
})

test_that("error is signaled when applying departial() to a non-function", {
  expect_errors_with_message(
    "Only functions can be de-partialized",
    departial(NULL),
    departial(~ function(x) NULL),
    departial(quo(function(x) NULL)),
    departial(quote(function(x) NULL))
  )
})

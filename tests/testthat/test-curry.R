expect_equal_formals <- function(f, g) {
  expect_equal(formals(f), formals(g))
}

context("Currying")

test_that("function without arguments is already curried", {
  f <- function() NULL
  expect_identical(curry(f), f)
  expect_true(is_curried(f))
})

test_that("function of a single (non-dots) argument is already curried", {
  fs <- list(function(x) NULL, function(x = "x") NULL)
  for (f in fs) {
    expect_identical(curry(f), f)
    expect_true(is_curried(f))
  }
})

test_that("function whose arguments all have values is already curried", {
  fs <- list(
    function() NULL,
    function(x = 1) NULL,
    function(x = 1, y = 2) NULL
  )
  for (f in fs) {
    expect_identical(curry(f), f)
    expect_true(is_curried(f))
  }
})

test_that("currying is idempotent", {
  fs <- list(
    function() NULL,
    function(x) NULL,
    function(x = "x") NULL,
    function(...) NULL,
    function(x, ...) NULL,
    function(x = "x", ...) NULL,
    function(x, y, ...) NULL,
    function(x, y = "y", ...) NULL,
    function(x = "x", y = "y", ...) NULL
  )
  for (f in fs) {
    fc <- curry(f)
    fcc <- curry(fc)
    fccc <- curry(fcc)
    expect_identical(fc, fcc)
    expect_identical(fc, fccc)

    # Identical, were it not for intervening ephemeral calling environments
    expect_equal(curry(f), curry(curry(f)))
    expect_equal(curry(f), curry(curry(curry(f))))
  }
})

test_that("function value is returned for a complete set of arguments", {
  f <- function(x) x
  fc <- curry(f)
  expect_equal(fc(1), f(1))

  f <- function(x = 0) x
  fc <- curry(f)
  expect_equal(fc(), f())
  expect_equal(fc(1), f(1))

  f <- function(x, y) c(x, y)
  fc <- curry(f)
  expect_equal(fc(1, 2), f(1, 2))

  f <- function(x, y = 2) c(x, y)
  fc <- curry(f)
  expect_equal(fc(1), f(1))
  expect_equal(fc(1, 2), f(1, 2))

  f <- function(x = 1, y = 2) c(x, y)
  fc <- curry(f)
  expect_equal(fc(1), f(1))
  expect_equal(fc(1, 2), f(1, 2))
})

test_that("curried function is returned for an incomplete set of arguments, without dots", {
  f <- function(u, v, w, x, y) c(u, v, w, x, y)

  fc <- curry(f)(1)
  expect_equal_formals(fc, function(v, w, x, y) {})
  expect_equal(fc(2, 3, 4, 5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2)
  expect_equal_formals(fc, function(w, x, y) {})
  expect_equal(fc(3, 4, 5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(2, u = 1)
  expect_equal_formals(fc, function(w, x, y) {})
  expect_equal(fc(3, 4, 5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2)
  expect_equal_formals(fc, function(w, x, y) {})
  expect_equal(fc(3, 4, 5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(v = 2)(1)
  expect_equal_formals(fc, function(w, x, y) {})
  expect_equal(fc(3, 4, 5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2, 3)
  expect_equal_formals(fc, function(x, y) {})
  expect_equal(fc(4, 5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2, 3)
  expect_equal_formals(fc, function(x, y) {})
  expect_equal(fc(4, 5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2)(3)
  expect_equal_formals(fc, function(x, y) {})
  expect_equal(fc(4, 5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2)(3)
  expect_equal_formals(fc, function(x, y) {})
  expect_equal(fc(4, 5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2, 3, 4)
  expect_equal_formals(fc, function(y) {})
  expect_equal(fc(5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2, 3)(4)
  expect_equal_formals(fc, function(y) {})
  expect_equal(fc(5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2)(3, 4)
  expect_equal_formals(fc, function(y) {})
  expect_equal(fc(5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2)(3)(4)
  expect_equal_formals(fc, function(y) {})
  expect_equal(fc(5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2, 3, 4)
  expect_equal_formals(fc, function(y) {})
  expect_equal(fc(5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2, 3)(4)
  expect_equal_formals(fc, function(y) {})
  expect_equal(fc(5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2)(3, 4)
  expect_equal_formals(fc, function(y) {})
  expect_equal(fc(5), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2)(3)(4)
  expect_equal_formals(fc, function(y) {})
  expect_equal(fc(5), c(1, 2, 3, 4, 5))
})

test_that("curried function is returned for an incomplete set of arguments, with dots", {
  f <- function(u, v, w, x, y, ...) c(u, v, w, x, y, ...)

  fc <- curry(f)(1)
  expect_equal_formals(fc, function(v, w, x, y, ...) {})
  expect_equal(fc(2, 3, 4, 5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2)
  expect_equal_formals(fc, function(w, x, y, ...) {})
  expect_equal(fc(3, 4, 5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(2, u = 1)
  expect_equal_formals(fc, function(w, x, y, ...) {})
  expect_equal(fc(3, 4, 5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2)
  expect_equal_formals(fc, function(w, x, y, ...) {})
  expect_equal(fc(3, 4, 5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(v = 2)(1)
  expect_equal_formals(fc, function(w, x, y, ...) {})
  expect_equal(fc(3, 4, 5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2, 3)
  expect_equal_formals(fc, function(x, y, ...) {})
  expect_equal(fc(4, 5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2, 3)
  expect_equal_formals(fc, function(x, y, ...) {})
  expect_equal(fc(4, 5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2)(3)
  expect_equal_formals(fc, function(x, y, ...) {})
  expect_equal(fc(4, 5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2)(3)
  expect_equal_formals(fc, function(x, y, ...) {})
  expect_equal(fc(4, 5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2, 3, 4)
  expect_equal_formals(fc, function(y, ...) {})
  expect_equal(fc(5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2, 3)(4)
  expect_equal_formals(fc, function(y, ...) {})
  expect_equal(fc(5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2)(3, 4)
  expect_equal_formals(fc, function(y, ...) {})
  expect_equal(fc(5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1, 2)(3)(4)
  expect_equal_formals(fc, function(y, ...) {})
  expect_equal(fc(5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2, 3, 4)
  expect_equal_formals(fc, function(y, ...) {})
  expect_equal(fc(5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2, 3)(4)
  expect_equal_formals(fc, function(y, ...) {})
  expect_equal(fc(5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2)(3, 4)
  expect_equal_formals(fc, function(y, ...) {})
  expect_equal(fc(5)(), c(1, 2, 3, 4, 5))

  fc <- curry(f)(1)(2)(3)(4)
  expect_equal_formals(fc, function(y, ...) {})
  expect_equal(fc(5)(), c(1, 2, 3, 4, 5))
})

test_that("dot-arguments can be set", {
  f <- function(x, y, z, ..., w = 4) c(x, y, z, w, ...)

  fc <- curry(f)(a = 5)
  expect_equal_formals(fc, f)
  expect_equal(fc(1, 2, 3)(), c(1, 2, 3, 4, a = 5))

  fc <- curry(f)(a = 5, b = 6)
  expect_equal_formals(fc, f)
  expect_equal(fc(1, 2, 3)(), c(1, 2, 3, 4, a = 5, b = 6))

  fc <- curry(f)(a = 5)(b = 6)
  expect_equal_formals(fc, f)
  expect_equal(fc(1, 2, 3)(), c(1, 2, 3, 4, a = 5, b = 6))

  fc <- curry(f)(1, a = 5)
  expect_equal_formals(fc, function(y, z, ..., w = 4) {})
  expect_equal(fc(2, 3)(), c(1, 2, 3, 4, a = 5))

  fc <- curry(f)(1)(a = 5)
  expect_equal_formals(fc, function(y, z, ..., w = 4) {})
  expect_equal(fc(2, 3)(), c(1, 2, 3, 4, a = 5))

  fc <- curry(f)(a = 5)(1)
  expect_equal_formals(fc, function(y, z, ..., w = 4) {})
  expect_equal(fc(2, 3)(), c(1, 2, 3, 4, a = 5))

  fc <- curry(f)(1, 2)(a = 5)
  expect_equal_formals(fc, function(z, ..., w = 4) {})
  expect_equal(fc(3)(), c(1, 2, 3, 4, a = 5))

  fc <- curry(f)(1)(2)(a = 5)
  expect_equal_formals(fc, function(z, ..., w = 4) {})
  expect_equal(fc(3)(), c(1, 2, 3, 4, a = 5))

  fc <- curry(f)(1)(a = 5)(2)
  expect_equal_formals(fc, function(z, ..., w = 4) {})
  expect_equal(fc(3)(), c(1, 2, 3, 4, a = 5))

  fc <- curry(f)(a = 5)(1)(2)
  expect_equal_formals(fc, function(z, ..., w = 4) {})
  expect_equal(fc(3)(), c(1, 2, 3, 4, a = 5))

  fc <- curry(f)(a = 5)(1, 2)
  expect_equal_formals(fc, function(z, ..., w = 4) {})
  expect_equal(fc(3)(), c(1, 2, 3, 4, a = 5))

  out <- curry(f)(1, 2, 3, a = 5, 6)()
  expect_equal(out, f(1, 2, 3, a = 5, 6))

  out <- curry(f)(a = 5, 1, 2, 3, 6)()
  expect_equal(out, f(a = 5, 1, 2, 3, 6))
})

test_that("formal argument is dropped once it is curried away", {
  f <- function(x, y = "y", ..., z = "z", w) NULL
  fc <- curry(f)

  expect_equal_formals(fc(0), function(y = "y", ..., z = "z", w) {})
  expect_equal_formals(fc(y = 0), function(x, ..., z = "z", w) {})
  expect_equal_formals(fc(z = 0), function(x, y = "y", ..., w) {})
  expect_equal_formals(fc(w = 0), function(x, y = "y", ..., z = "z") {})
})

test_that("dots persist", {
  f <- function(u, v, w = "w", ..., x = "x", y) NULL
  fc <- curry(f)
  fs <- list(
    fc(1),
    fc(1, 2),
    fc(1)(2),
    fc(1, 2, 3),
    fc(1, 2)(3),
    fc(1)(2, 3),
    fc(1)(2)(3),
    fc(1, 2, 3, 4),
    fc(1, 2, 3, 4, a = 5),
    fc(x = "x", 1, 2, 3, 4, a = 5),
    fc(y = "y", u = 1, a = 5),
    fc(1, 2, 3, y = "y")
  )
  for (f in fs) {
    expect_true("..." %in% names(formals(f)))
    expect_true(is_curried(f))
  }
})

context("Uncurrying")

test_that("uncurried function is only callable on a complete set of arguments", {
  fc <- curry(function(x, y, ..., z) c(x, y, z, ...))

  fu <- uncurry(fc(1))
  expect_equal_formals(fu, function(y, ..., z) {})
  expect_equal(fu(2, z = 3), c(1, 2, 3))
  expect_equal(fu(2, z = 3), c(1, 2, 3))
  expect_error(fu(2), "argument \"z\" is missing")
  expect_error(fu(z = 3), "argument \"y\" is missing")

  fu <- uncurry(fc(1, 2))
  expect_equal_formals(fu, function(..., z) {})
  expect_equal(fu(z = 3), c(1, 2, 3))
  expect_error(fu(3), "argument \"z\" is missing")

  fu <- uncurry(fc(z = 3))
  expect_equal_formals(fu, function(x, y, ...) {})
  expect_equal(fu(1, 2), c(1, 2, 3))
  expect_error(fu(1), "argument \"y\" is missing")
  expect_error(fu(y = 2), "argument \"x\" is missing")

  fu <- uncurry(fc(a = 1))
  expect_equal_formals(fu, function(x, y, ..., z) {})
  expect_equal(fu(1, 2, z = 3), c(1, 2, 3, a = 1))
  expect_error(fu(1, 2), "argument \"z\" is missing")
  expect_error(fu(1, z = 3), "argument \"y\" is missing")
  expect_error(fu(y = 2, z = 3), "argument \"x\" is missing")
})

test_that("error signaled when attempting to uncurry a non-function", {
  non_fs <- list(NULL, quote(function(x) NULL), quote(identity))
  for (x in non_fs)
    expect_error(uncurry(x), "Only functions can be uncurried")
})

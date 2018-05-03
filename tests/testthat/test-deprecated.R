context("Deprecated functions")

expect_warning_literal <- function(object, regexp, ...) {
  expect_warning(object, regexp, fixed = TRUE, ...)
}

test_that("as_fn() is deprecated", {
  expect_warning_literal(
    as_fn(identity),
    "`as_fn()` is deprecated."
  )
})

test_that("make_fn_aware() is deprecated", {
  expect_warning_literal(
    make_fn_aware(identity),
    "`make_fn_aware()` is deprecated."
  )
})

test_that("curry() is deprecated", {
  expect_warning_literal(
    curry(identity),
    "`curry()` is deprecated. Instead, apply `partial()` (iteratively)"
  )
})

test_that("curry_fn() is deprecated", {
  expect_warning_literal(
    curry_fn(~NULL),
    "`curry_fn()` is deprecated. Instead, apply `partial()` (iteratively)."
  )
})

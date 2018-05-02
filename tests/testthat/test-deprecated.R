context("Deprecated functions")

test_that("as_fn() is deprecated", {
  expect_warning(
    as_fn(identity),
    "'as_fn' is deprecated"
  )
})

test_that("make_fn_aware() is deprecated", {
  expect_warning(
    make_fn_aware(identity),
    "'make_fn_aware' is deprecated"
  )
})

test_that("curry() is deprecated", {
  expect_warning(
    curry(identity),
    "'curry' is deprecated. Instead, apply 'partial' repeatedly."
  )
})

test_that("curry_fn() is deprecated", {
  expect_warning(
    curry_fn(~NULL),
    "'curry_fn' is deprecated. Instead, apply 'partial' repeatedly."
  )
})

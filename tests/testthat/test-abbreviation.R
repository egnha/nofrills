context("Functional abbreviation")

foo <- function(f, g, x, ...) f(g(x), ...)
bar <- function(...) list(...)

test_that("functional abbreviation interprets functions literally", {
  expect_identical(
    make_fn_aware(foo, "f")(bar, identity, 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    make_fn_aware(foo, "g")(bar, identity, 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    make_fn_aware(foo, "f", "g")(bar, identity, 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    make_fn_aware(foo, "g", "f")(bar, identity, 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
})

test_that("functional abbreviation interprets dot-abbreviations", {
  expect_identical(
    make_fn_aware(foo, "f")(.(... ~ list(...)), identity, 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    make_fn_aware(foo, "g")(bar, .(. ~ .), 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    make_fn_aware(foo, "f", "g")(.(... ~ list(...)), .(. ~ .), 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    make_fn_aware(foo, "g", "f")(.(... ~ list(...)), .(. ~ .), 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
})

test_that("f is simply returned, if ... is empty and f is a function", {
  expect_identical(make_fn_aware(foo), foo)
  expect_identical(make_fn_aware(foo, NULL), foo)
  expect_identical(make_fn_aware(foo, character(0)), foo)
})

test_that("f is called from same environment as make_fn_aware(f, ...)()", {
  foo <- function(bar) parent.frame()
  expect_identical(make_fn_aware(f = foo, "bar")(), environment())
  expect_identical(make_fn_aware(f = foo, "bar")(), foo())
})

test_that("can splice in argument names", {
  expect_equal(
    make_fn_aware(foo, "f", "g"),
    make_fn_aware(foo, !!! list("f", "g"))
  )
  expect_equal(
    make_fn_aware(foo, "f"),
    make_fn_aware(foo, !!! list("f"))
  )
  expect_equal(
    make_fn_aware(foo),
    make_fn_aware(foo, !!! list())
  )
  expect_equal(
    make_fn_aware(foo, "f", "g"),
    make_fn_aware(foo, UQS(list("f", "g")))
  )
  expect_equal(
    make_fn_aware(foo, "f"),
    make_fn_aware(foo, UQS(list("f")))
  )
  expect_equal(
    make_fn_aware(foo),
    make_fn_aware(foo, UQS(list()))
  )
})

test_that("error is signaled if f is not a function or abbreviation thereof", {
  msg <- "object 'non_existent' of mode 'function' was not found"
  expect_error(make_fn_aware("non_existent"), msg)
  expect_error(make_fn_aware("non_existent", "f"), msg)
})

test_that("error is signaled if ... are neither strings nor empty", {
  expect_error(make_fn_aware(foo, f), "object 'f' not found")
  expect_error(make_fn_aware(foo, 1), "Can't convert a double vector")
})

test_that("error is signaled if ... are not names of arguments of f", {
  msg <- "Invalid argument name\\(s\\)"
  expect_error(make_fn_aware(foo, "a"), msg)
  expect_error(make_fn_aware(foo, "a", "f"), msg)
  expect_error(make_fn_aware(foo, "f", "a"), msg)
})

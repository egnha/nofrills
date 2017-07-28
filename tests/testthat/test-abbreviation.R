context("Functional abbreviation")

foo <- function(f, g, x, ...) f(g(x), ...)
bar <- function(...) list(...)

test_that("functional abbreviation interprets functions literally", {
  expect_identical(
    abbrev_fn_args(foo, "f")(bar, identity, 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    abbrev_fn_args(foo, "g")(bar, identity, 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    abbrev_fn_args(foo, "f", "g")(bar, identity, 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    abbrev_fn_args(foo, "g", "f")(bar, identity, 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
})

test_that("functional abbreviation interprets dot-abbreviations", {
  expect_identical(
    abbrev_fn_args(foo, "f")(.(... ~ list(...)), identity, 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    abbrev_fn_args(foo, "g")(bar, .(. ~ .), 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    abbrev_fn_args(foo, "f", "g")(.(... ~ list(...)), .(. ~ .), 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
  expect_identical(
    abbrev_fn_args(foo, "g", "f")(.(... ~ list(...)), .(. ~ .), 1, "2", NULL, NA),
    foo(bar, identity, 1, "2", NULL, NA)
  )
})

test_that("f is simply returned, if ... is empty and f is a function", {
  expect_identical(abbrev_fn_args(foo), foo)
  expect_identical(abbrev_fn_args(foo, NULL), foo)
  expect_identical(abbrev_fn_args(foo, character(0)), foo)
})

test_that("f is called from same environment as abbrev_fn_args(f, ...)()", {
  foo <- function(bar) parent.frame()
  expect_identical(abbrev_fn_args(f = foo, "bar")(), environment())
  expect_identical(abbrev_fn_args(f = foo, "bar")(), foo())
})

test_that("can splice in argument names", {
  expect_equal(
    abbrev_fn_args(foo, "f", "g"),
    abbrev_fn_args(foo, !!! list("f", "g"))
  )
  expect_equal(
    abbrev_fn_args(foo, "f"),
    abbrev_fn_args(foo, !!! list("f"))
  )
  expect_equal(
    abbrev_fn_args(foo),
    abbrev_fn_args(foo, !!! list())
  )
  expect_equal(
    abbrev_fn_args(foo, "f", "g"),
    abbrev_fn_args(foo, UQS(list("f", "g")))
  )
  expect_equal(
    abbrev_fn_args(foo, "f"),
    abbrev_fn_args(foo, UQS(list("f")))
  )
  expect_equal(
    abbrev_fn_args(foo),
    abbrev_fn_args(foo, UQS(list()))
  )
})

test_that("error is signaled if f is not a function or abbreviation thereof", {
  msg <- "object 'non_existent' of mode 'function' was not found"
  expect_error(abbrev_fn_args("non_existent"), msg)
  expect_error(abbrev_fn_args("non_existent", "f"), msg)
})

test_that("error is signaled if ... are neither strings nor empty", {
  expect_error(abbrev_fn_args(foo, f), "object 'f' not found")
  expect_error(abbrev_fn_args(foo, 1), "Can't convert a double vector")
})

test_that("error is signaled if ... are not names of arguments of f", {
  msg <- "Invalid argument name\\(s\\)"
  expect_error(abbrev_fn_args(foo, "a"), msg)
  expect_error(abbrev_fn_args(foo, "a", "f"), msg)
  expect_error(abbrev_fn_args(foo, "f", "a"), msg)
})

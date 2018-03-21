context("Quasiquotation")

context("fn()")

test_that("default values can be unquoted", {
  zero <- 0
  expect_equal(fn(x = !!zero ~ NULL), function(x = 0) NULL)
})

test_that("arguments names can be unquoted", {
  arg <- "x"
  expect_equal(fn(!!arg := 0 ~ NULL), function(x = 0) NULL)
})

test_that("arguments can be unquoted, as symbols", {
  arg <- as.name("x")
  expect_equal(fn(!!arg ~ NULL), function(x) NULL)
})

test_that("formals can be spliced in as arguments", {
  f <- function(x, y = 1, ..., z = x + y) x + y + z
  fmls <- formals(f)
  expect_equal_(fn(!!!fmls, ~ x + y + z), f)
  expect_equal_(fn(!!!formals(f), ~ x + y + z), f)
})

test_that("function body can be unquoted", {
  f <- function(x) {
    one <- 1
    x + one
  }
  expect_equal(fn(x ~ !!body(f)), f)
})

test_that("unquoting operators can be literally expressed", {
  expect_equal(fn(x = foo(QUQ(y)) ~ NULL), function(x = foo(`!!`(y))) NULL)
  expect_equal(fn(x = foo(QUQS(y)) ~ NULL), function(x = foo(`!!!`(y))) NULL)
  expect_equal_(fn(x ~ foo(QUQ(x))), function(x) foo(`!!`(x)))
  expect_equal_(fn(x ~ foo(QUQS(x))), function(x) foo(`!!!`(x)))
})

context("as_fn()")

foo <- function(x) suppressWarnings(as_fn(x))

test_that("default values can be unquoted", {
  zero <- 0
  expect_equal(foo(.(x = !!zero ~ NULL)), function(x = 0) NULL)
})

test_that("arguments names can be unquoted", {
  arg <- "x"
  expect_equal(foo(.(!!arg := 0 ~ NULL)), function(x = 0) NULL)
})

test_that("arguments can be unquoted, as symbols", {
  arg <- as.name("x")
  expect_equal(foo(.(!!arg ~ NULL)), function(x) NULL)
})

test_that("formals can be spliced in as arguments", {
  f <- function(x, y = 1, ..., z = x + y) x + y + z
  fmls <- formals(f)
  expect_equal_(foo(.(!!!fmls, ~ x + y + z)), f)
  expect_equal_(foo(.(!!!formals(f), ~ x + y + z)), f)
})

test_that("function body can be unquoted", {
  f <- function(x) {
    one <- 1
    x + one
  }
  expect_equal(foo(.(x ~ !!body(f))), f)
})

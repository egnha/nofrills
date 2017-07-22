context("Quasiquotation")

context("fn()")

test_that("default values can be unquoted", {
  zero <- 0
  expect_equal(fn(x = !! zero ~ NULL), function(x = 0) NULL)
  expect_equal(fn(x = UQ(zero) ~ NULL), function(x = 0) NULL)
})

test_that("arguments names can be unquoted", {
  arg <- "x"
  expect_equal(fn(!! arg := 0 ~ NULL), function(x = 0) NULL)
  expect_equal(fn(UQ(arg) := 0 ~ NULL), function(x = 0) NULL)
})

test_that("arguments can be unquoted, as symbols", {
  arg <- as.name("x")
  expect_equal(fn(!! arg ~ NULL), function(x) NULL)
  expect_equal(fn(UQ(arg) ~ NULL), function(x) NULL)
})

test_that("formals can be spliced in as arguments", {
  f <- function(x, y = 1, ..., z = x + y) x + y + z
  fmls <- formals(f)
  expect_equal(fn(!!! fmls, ~ x + y + z), f)
  expect_equal(fn(UQS(fmls), ~ x + y + z), f)
  expect_equal(fn(!!! formals(f), ~ x + y + z), f)
  expect_equal(fn(UQS(formals(f)), ~ x + y + z), f)
})

test_that("function body can be unquoted", {
  f <- function(x) {
    one <- 1
    x + one
  }
  expect_equal(fn(x ~ !! body(f)), f)
  expect_equal(fn(x ~ UQ(body(f))), f)
})

context("as_fn()")

foo <- function(x) as_fn(x)

test_that("default values can be unquoted", {
  zero <- 0
  expect_equal(foo(.(x = !! zero ~ NULL)), function(x = 0) NULL)
  expect_equal(foo(.(x = UQ(zero) ~ NULL)), function(x = 0) NULL)
})

test_that("arguments names can be unquoted", {
  arg <- "x"
  expect_equal(foo(.(!! arg := 0 ~ NULL)), function(x = 0) NULL)
  expect_equal(foo(.(UQ(arg) := 0 ~ NULL)), function(x = 0) NULL)
})

test_that("arguments can be unquoted, as symbols", {
  arg <- as.name("x")
  expect_equal(foo(.(!! arg ~ NULL)), function(x) NULL)
  expect_equal(foo(.(UQ(arg) ~ NULL)), function(x) NULL)
})

test_that("formals can be spliced in as arguments", {
  f <- function(x, y = 1, ..., z = x + y) x + y + z
  fmls <- formals(f)
  expect_equal(foo(.(!!! fmls, ~ x + y + z)), f)
  expect_equal(foo(.(UQS(fmls), ~ x + y + z)), f)
  expect_equal(foo(.(!!! formals(f), ~ x + y + z)), f)
  expect_equal(foo(.(UQS(formals(f)), ~ x + y + z)), f)
})

test_that("function body can be unquoted", {
  f <- function(x) {
    one <- 1
    x + one
  }
  expect_equal(foo(.(x ~ !! body(f))), f)
  expect_equal(foo(.(x ~ UQ(body(f)))), f)
})

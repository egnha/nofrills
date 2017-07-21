context("Quasiquotation")

context("eff()")

test_that("default values can be unquoted", {
  zero <- 0
  expect_equal(eff(x = !! zero ~ NULL), function(x = 0) NULL)
  expect_equal(eff(x = UQ(zero) ~ NULL), function(x = 0) NULL)
})

test_that("arguments names can be unquoted", {
  arg <- "x"
  expect_equal(eff(!! arg := 0 ~ NULL), function(x = 0) NULL)
  expect_equal(eff(UQ(arg) := 0 ~ NULL), function(x = 0) NULL)
})

test_that("arguments can be unquoted, as symbols", {
  arg <- as.name("x")
  expect_equal(eff(!! arg ~ NULL), function(x) NULL)
  expect_equal(eff(UQ(arg) ~ NULL), function(x) NULL)
})

test_that("formals can be spliced in as arguments", {
  f <- function(x, y = 1, ..., z = x + y) x + y + z
  expect_equal(eff(!!! formals(f), ~ x + y + z), f)
  expect_equal(eff(UQS(formals(f)), ~ x + y + z), f)
})

test_that("function body can be unquoted", {
  f <- function(x) {
    one <- 1
    x + one
  }
  expect_equal(eff(x ~ !! body(f)), f)
  expect_equal(eff(x ~ UQ(body(f))), f)
})

context("as_eff()")

foo <- function(x) as_eff(x)

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

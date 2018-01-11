context("Currying")

test_that("functions can be curried", {
  expect_equal(curry(function() NULL), function() NULL)
  expect_equal(curry(function(x) x), function(x) x)
  expect_equal(curry(function(x, y) x + y), function(x) function(y) x + y)
  expect_equal(curry(function(x = 0, y) x + y), function(x = 0) function(y) x + y)
  expect_equal(curry(function(x, y = 0) x + y), function(x) function(y = 0) x + y)
  expect_equal(curry(function(x, y, z) x + y + z), function(x) function(y) function(z) x + y + z)
})

test_that("function declarations can be curried", {
  expect_equal(curry_fn(~NULL), function() NULL)
  expect_equal(curry_fn(x ~ x), function(x) x)
  expect_equal(curry_fn(x, y ~ x + y), function(x) nofrills::fn(y = , ~x + y))
  expect_equal(curry_fn(x = 0, y ~ x + y), function(x = 0) nofrills::fn(y = , ~x + y))
  expect_equal(curry_fn(x, y = 0 ~ x + y), function(x) nofrills::fn(y = 0, ~x + y))
  expect_equal(curry_fn(x, y, z ~ x + y + z), function(x) function(y) nofrills::fn(z = , ~x + y + z))
})

test_that("dots (...) are treated as a single argument", {
  expect_equal(curry(function(...) NULL), function(...) NULL)
  expect_equal(curry(function(x, ...) NULL), function(x) function(...) NULL)
  expect_equal(curry(function(..., x) NULL), function(...) function(x) NULL)
  expect_equal(curry_fn(... ~ NULL), function(...) NULL)
  expect_equal(curry_fn(x, ... ~ NULL), function(x) nofrills::fn(... = , ~NULL))
  expect_equal(curry_fn(... = , x ~ NULL), function(...) nofrills::fn(x = , ~NULL))
})

test_that("function environment can be set", {
  env <- new.env()
  expect_equal(environment(curry(function(x, y) NULL, env)), env)
  expect_equal(environment(curry_fn(x, y ~ NULL, ..env = env)), env)
})

test_that("function environment is preserved", {
  env <- new.env()
  foo <- evalq(function(x, y) NULL, env)
  expect_equal(environment(curry(foo)), env)
})

test_that("curried function declarations support argument value unquoting", {
  zero <- 0
  expect_equal(curry_fn(x = !!zero, y ~ NULL), function(x = 0) nofrills::fn(y = , ~NULL))
})

test_that("curried function declarations support argument name unquoting", {
  arg <- "x"
  expect_equal(curry_fn(!!arg := 0, y ~ NULL), function(x = 0) nofrills::fn(y = , ~NULL))
})

test_that("curried function declarations support argument splicing", {
  f <- function(x, y = 1, ..., z) x + y + z
  f_curried <- curry_fn(x, y = 1, ... = , z ~ x + y + z)
  fmls <- formals(f)
  expect_equal_(curry_fn(!!!fmls, ~x + y + z), f_curried)
  expect_equal_(curry_fn(!!!formals(f), ~x + y + z), f_curried)
})

test_that("curried function declarations support body unquoting", {
  f <- function(x, y) x + y
  f_curried <- curry_fn(x, y ~ x + y)
  expect_equal(curry_fn(x, y ~ !!body(f)), f_curried)
})

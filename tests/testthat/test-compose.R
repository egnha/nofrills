fs <- lapply(1:3, function(i) function(. = NULL) c(i, .))
cmp <- function() fs[[1]](fs[[2]](fs[[3]]()))
cmps <- list(
  # Conventional composition
  compose(fs[[1]], fs[[2]], fs[[3]]),
  compose(fs[[1]], compose(fs[[2]], fs[[3]])),
  compose(compose(fs[[1]], fs[[2]]), fs[[3]]),
  # Backward composition
   fs[[1]] %<<<%  fs[[2]]  %<<<% fs[[3]],
  (fs[[1]] %<<<%  fs[[2]]) %<<<% fs[[3]],
   fs[[1]] %<<<% (fs[[2]]  %<<<% fs[[3]]),
  # Forward composition
   fs[[3]] %>>>%  fs[[2]]  %>>>% fs[[1]],
  (fs[[3]] %>>>%  fs[[2]]) %>>>% fs[[1]],
   fs[[3]] %>>>% (fs[[2]]  %>>>% fs[[1]])
)

context("Composing functions")

test_that("NULL is returned when no functions are composed", {
  expect_identical(compose(), NULL)
})

test_that("single function is returned, unchanged, when composing", {
  f <- function() NULL
  expect_identical(compose(f), f)
})

test_that("composition is associative", {
  expect_identical(cmp(), 1:3)
  for (assoc in cmps)
    expect_identical(assoc(), cmp())
})

test_that("list of functions can be spliced", {
  expect_identical(compose(!!! fs)(), cmp())
})

test_that("composition has formals of innermost function (as a closure)", {
  outer <- function(.) NULL
  inner <- function(x, y, ..., z = "default") NULL
  expect_identical(formals(compose(outer, inner)), formals(inner))
  expect_identical(formals(compose(outer, log)), formals(rlang::as_closure(log)))
})

context("Decomposing compositions")

test_that("single function is returned, unchanged, when decomposing", {
  expect_identical(decompose(sin), sin)
  expect_identical(decompose(function() NULL), function() NULL)
})

test_that("error is signalled when decomposing a non-function", {
  errmsg <- "Only functions can be \\(de\\)composed"
  expect_error(decompose(NULL), errmsg)
  expect_error(decompose(quote(function() {})), errmsg)
})

test_that("list of composite functions is flat", {
  for (assoc in cmps)
    expect_equivalent(decompose(assoc), fs)
})

test_that("decompose() inverts compose()", {
  expect_error(decompose(compose()), "Only functions can be \\(de\\)composed")
  expect_equivalent(decompose(compose(fs[[1]])), fs[[1]])
  expect_equivalent(decompose(compose(fs[[1]], fs[[2]], fs[[3]])), fs)
})

test_that("compose() inverts decompose()", {
  expect_identical(compose(decompose(fs[[1]])), fs[[1]])
  cmp <- compose(fs[[1]], fs[[2]], fs[[3]])
  expect_equivalent(compose(!!! decompose(cmp)), cmp)
})

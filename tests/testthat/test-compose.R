make_funs <- function(n)
  lapply(seq_len(n), function(i) function(. = NULL) c(i, .))
fs <- make_funs(3)
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

test_that("for a single function, composition is identity", {
  for (f in list(closure = identity, special = log, builtin = c))
    expect_identical(compose(f), f)
})

test_that("error is signalled when composing a non-function", {
  errmsg <- "Only functions can be composed"
  expect_error(compose(), errmsg)
  expect_error(compose(NULL), errmsg)
  expect_error(compose(list()), errmsg)
  expect_error(compose(quote(function() {})), errmsg)
  expect_error(compose(identity, quote(function() {})), errmsg)
})

test_that("composition is associative", {
  expect_identical(cmp(), 1:3)
  for (assoc in cmps)
    expect_identical(assoc(), cmp())
})

test_that("nested compositions are flattened", {
  gs <- make_funs(4)
  cmps <- list(
    compose(gs[[1]], gs[[2]], gs[[3]], gs[[4]]),
    compose(gs[[1]], compose(gs[[2]], gs[[3]], gs[[4]])),
    compose(gs[[1]], compose(gs[[2]], compose(gs[[3]], gs[[4]]))),
    compose(gs[[1]], compose(gs[[2]], compose(gs[[3]], compose(gs[[4]]))))
  )
  for (cmp in cmps)
    expect_equivalent(decompose(cmp), gs)
})

test_that("list of functions can be spliced", {
  expect_identical(compose(fs)(), cmp())
})

test_that("list of functions can be spliced using `!!!`", {
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
  errmsg <- "Only functions can be decomposed"
  expect_error(decompose(NULL), errmsg)
  expect_error(decompose(list(identity)), errmsg)
  expect_error(decompose(quote(function() {})), errmsg)
})

test_that("list of composite functions is flat", {
  for (assoc in cmps)
    expect_equivalent(decompose(assoc), fs)
})

test_that("decompose() inverts compose()", {
  expect_equivalent(decompose(compose(fs[[1]])), fs[[1]])
  expect_equivalent(decompose(compose(fs[[1]], fs[[2]], fs[[3]])), fs)
})

test_that("compose() inverts decompose()", {
  expect_identical(compose(decompose(fs[[1]])), fs[[1]])
  cmp <- compose(fs[[1]], fs[[2]], fs[[3]])
  expect_equivalent(compose(!!! decompose(cmp)), cmp)
})

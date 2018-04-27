make_funs <- function(n) {
  lapply(seq_len(n), function(i) {force(i); function(. = NULL) c(i, .)})
}

fs <- make_funs(3)

fn_kinds <- list(
  closure     = identity,
  special     = log,
  builtin     = c,
  composition = compose(fs)
)

cmp <- function() {
  fs[[1]](fs[[2]](fs[[3]]()))
}

cmps <- list(
  # Conventional composition
  compose(fs[[1]], compose(fs[[2]], compose(fs[[3]]))),
  compose(compose(fs[[1]], compose(fs[[2]])), fs[[3]]),
  compose(fs[[1]], compose(fs[[2]], fs[[3]])),
  compose(compose(fs[[1]], fs[[2]]), fs[[3]]),
  compose(compose(fs[[1]], fs[[2]], fs[[3]])),
  compose(fs[[1]], fs[[2]], fs[[3]]),
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
  for (f in fn_kinds)
    expect_equal(compose(f), f)
})

test_that("error is signalled when composing an empty sequence of functions", {
  expect_errors_with_message(
    "Must specify functions to compose",
    compose(),
    compose(NULL),
    compose(list()),
    compose(!!! list())
  )
})

test_that("NULL is void in a sequence of functions", {
  expect_error(compose(NULL), "Must specify functions to compose")

  expect_identical(compose(NULL, identity), identity)
  expect_identical(compose(identity, NULL), identity)

  inc <- function(x) x + 1
  cmp0 <- compose(log, inc)
  cmps <- list(
    compose(NULL, log, inc),
    compose(log, NULL, inc),
    compose(log, inc, NULL)
  )

  # Function equality by equality of return values
  vals <- {set.seed(1); runif(10)}
  for (cmp in cmps)
      expect_equal(cmp(vals), cmp0(vals))
})

test_that("error is signalled when composing a non-interpretable object", {
  errmsg <- function(x) {
    cls <- paste(deparse(class(x)), collapse = "")
    sprintf("Cannot interpret object of class %s as a function", cls)
  }

  noninterp <- list(
    as.data.frame(1:3),
    quote(x),
    quote(function() NULL),
    structure(NA, class = "foo")
  )

  for (obj in noninterp) {
    msg <- errmsg(obj)
    expect_error(compose(obj), msg)
    expect_error(compose(identity, obj), msg)
    expect_error(compose(obj, identity), msg)
  }
})

test_that("composition is associative", {
  value <- cmp()
  expect_identical(value, 1:3)
  for (assoc in cmps)
    expect_identical(assoc(), value)
})

test_that("nested compositions are flattened", {
  gs <- make_funs(4)

  # Test by call
  cmps <- list(
    compose(gs[[1]], gs[[2]], gs[[3]], gs[[4]]),
    compose(compose(gs[[1]], gs[[2]], gs[[3]], gs[[4]])),
    compose(gs[[1]] %<<<% gs[[2]] %<<<% gs[[3]] %<<<% gs[[4]]),
    compose(gs[[4]] %>>>% gs[[3]] %>>>% gs[[2]] %>>>% gs[[1]]),
    compose(gs[[1]], compose(gs[[2]], gs[[3]], gs[[4]])),
    compose(gs[[1]] %<<<% compose(gs[[2]], gs[[3]], gs[[4]])),
    compose(compose(gs[[2]], gs[[3]], gs[[4]]) %>>>% gs[[1]]),
    compose(gs[[1]], compose(gs[[2]], gs[[3]] %<<<% gs[[4]])),
    compose(gs[[1]], compose(gs[[2]], gs[[4]] %>>>% gs[[3]])),
    compose(gs[[1]], compose(gs[[2]], compose(gs[[3]], gs[[4]]))),
    compose(gs[[1]], gs[[2]] %<<<% compose(gs[[3]], gs[[4]])),
    compose(gs[[1]], compose(gs[[3]], gs[[4]]) %>>>% gs[[2]]),
    compose(gs[[1]], compose(gs[[2]], compose(gs[[3]], compose(gs[[4]])))),
    compose(gs[[1]] %<<<% (gs[[2]] %<<<% (gs[[3]] %<<<% gs[[4]]))),
    compose(((gs[[4]] %>>>% gs[[3]]) %>>>% gs[[2]]) %>>>% gs[[1]])
  )
  for (cmp in cmps)
    expect_equivalent(decompose(cmp), gs)

  # Test by value
  cmps <- Reduce(compose, gs, accumulate = TRUE)
  for (i in seq_along(gs))
    expect_equivalent(decompose(cmps[[i]]), gs[seq_len(i)])
})

test_that("list of functions can be spliced", {
  expect_equal(compose(fs), compose(fs[[1]], fs[[2]], fs[[3]]))
  expect_identical(compose(fs)(), cmp())
})

test_that("list of functions can be spliced using `!!!`", {
  expect_equal(compose(!!! fs), compose(fs[[1]], fs[[2]], fs[[3]]))
  expect_identical(compose(!!! fs)(), cmp())
})

test_that("composition has formals of innermost function (as a closure)", {
  outer <- function(.) NULL
  fs <- list(
    closure = function(x, y, ..., z = "default") NULL,
    special = log,
    builtin = c
  )
  for (inner in fs) {
    fmls_inner <- formals(rlang::as_closure(inner))
    expect_identical(formals(compose(outer, inner)), fmls_inner)
  }
})

test_that("environment of composition is child of initial-function environment", {
  fs <- c(
    fn_kinds[names(fn_kinds) != "composition"],
    local(function() NULL)
  )
  for (f in fs) {
    cmp <- compose(identity, f)
    env <- if (is.null(environment(f))) baseenv() else environment(f)
    expect_identical(parent.env(environment(cmp)), env)
  }
})

test_that("one-sided formula of a function is lifted", {
  div <- function(a, b) a / b
  cmp <- compose(~div, list)
  vals <- {set.seed(1); runif(10)}
  expect_equal(cmp(vals, vals + 1), div(vals, vals + 1))
})

test_that("(boolean) filter length must equal input length (#36)", {
  f <- compose(c(T, F, T), list)
  g <- compose(c(a = T, b = F, c = T), list)

  expect_equal(f(1, 2, 3), list(1, 3))
  expect_equal(g(1, 2, 3), list(a = 1, c = 3))

  expect_error(f(1, 2), "Filter length \\(3\\) must equal input length \\(2\\)")
  expect_error(g(1, 2), "Filter length \\(3\\) must equal input length \\(2\\)")
})

test_that("selectors dispatch `[` from calling environment (#37)", {
  `[.SomeClass` <- function(x, i) {
    message("SomeClass")
    x <- x + 1
    NextMethod(`[`)
  }

  x <- structure(c(1, 2, b = 3, 4), class = "SomeClass")
  fs <- list(
    compose(c(a = T, F, T, F), identity),
    compose(c(a = 1, 3), identity)
  )

  for (f in fs) {
    expect_equal(f(x), c(a = 2, b = 4))
    expect_message(f(x), "SomeClass")
  }
})

context("Decomposing compositions")

test_that("decomposing a non-composite function wraps it in a list", {
  for (f in fn_kinds[c("closure", "special", "builtin")])
    expect_identical(decompose(f), list(f))
})

test_that("error is signalled when decomposing a non-function", {
  expect_errors_with_message(
    "Only functions can be decomposed",
    decompose(NULL),
    decompose(~function() NULL),
    decompose(quote(function() NULL)),
    decompose(list(function() NULL))
  )
})

test_that("list of composite functions is flat", {
  for (assoc in cmps)
    expect_equivalent(decompose(assoc), fs)
})

test_that("decompose() inverts compose()", {
  expect_equivalent(decompose(compose(fs)), fs)
  expect_equivalent(decompose(compose(fs[[1]], fs[[2]], fs[[3]])), fs)
  expect_equivalent(decompose(compose(fs[[1]], fs[[2]])), fs[1:2])
  expect_equivalent(decompose(compose(fs[[1]])), fs[1])
})

test_that("compose() inverts decompose()", {
  # Test by call
  expect_equal(compose(decompose(compose(fs))), compose(fs))

  # Test by value
  for (f in fn_kinds)
    expect_equal(compose(decompose(f)), f)
})

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
   {fs[[1]]} %<<<%  {fs[[2]]}  %<<<% {fs[[3]]},
  ({fs[[1]]} %<<<%  {fs[[2]]}) %<<<% {fs[[3]]},
   {fs[[1]]} %<<<% ({fs[[2]]}  %<<<% {fs[[3]]}),
  # Forward composition
   {fs[[3]]} %>>>%  {fs[[2]]}  %>>>% {fs[[1]]},
  ({fs[[3]]} %>>>%  {fs[[2]]}) %>>>% {fs[[1]]},
   {fs[[3]]} %>>>% ({fs[[2]]}  %>>>% {fs[[1]]})
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
  vals <- {set.seed(1); runif(10)}

  # Function equality by equality of return values
  for (cmp in cmps)
    for (val in vals)
      expect_equal(cmp(val), cmp0(val))
})

test_that("error is signalled when composing a non-decomposable object", {
  errmsg <- function(x) {
    cls <- paste(deparse(class(x)), collapse = "")
    sprintf("Cannot decompose object of class %s", cls)
  }

  nondecomposable <- list(
    as.data.frame(1:3),
    quote(x),
    quote(function() NULL),
    structure(NA, class = "foo")
  )

  for (obj in nondecomposable) {
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
    compose({gs[[1]]} %<<<% {gs[[2]]} %<<<% {gs[[3]]} %<<<% {gs[[4]]}),
    compose({gs[[4]]} %>>>% {gs[[3]]} %>>>% {gs[[2]]} %>>>% {gs[[1]]}),
    compose(gs[[1]], compose(gs[[2]], gs[[3]], gs[[4]])),
    compose({gs[[1]]} %<<<% {compose(gs[[2]], gs[[3]], gs[[4]])}),
    compose({compose(gs[[2]], gs[[3]], gs[[4]])} %>>>% {gs[[1]]}),
    compose(gs[[1]], compose(gs[[2]], {gs[[3]]} %<<<% {gs[[4]]})),
    compose(gs[[1]], compose(gs[[2]], {gs[[4]]} %>>>% {gs[[3]]})),
    compose(gs[[1]], compose(gs[[2]], compose(gs[[3]], gs[[4]]))),
    compose(gs[[1]], {gs[[2]]} %<<<% {compose(gs[[3]], gs[[4]])}),
    compose(gs[[1]], {compose(gs[[3]], gs[[4]])} %>>>% {gs[[2]]}),
    compose(gs[[1]], compose(gs[[2]], compose(gs[[3]], compose(gs[[4]])))),
    compose({gs[[1]]} %<<<% ({gs[[2]]} %<<<% ({gs[[3]]} %<<<% {gs[[4]]}))),
    compose((({gs[[4]]} %>>>% {gs[[3]]}) %>>>% {gs[[2]]}) %>>>% {gs[[1]]})
  )
  for (cmp in cmps)
    expect_equivalent(decompose(cmp), gs)

  # Test by value
  cmps <- Reduce(compose, gs, accumulate = TRUE)
  expect_equivalent(decompose(cmps[[1]]), gs[[1]])
  for (i in seq_along(gs)[-1])
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
  inner <- function(x, y, ..., z = "default") NULL
  expect_identical(formals(compose(outer, inner)), formals(inner))
  expect_identical(formals(compose(outer, log)), formals(rlang::as_closure(log)))
  expect_identical(formals(compose(outer, c)), formals(rlang::as_closure(c)))
})

test_that("environment of composition is child of initial-function environment", {
  fns <- c(
    fn_kinds[names(fn_kinds) != "composition"],
    local(function() NULL)
  )
  for (f in fns) {
    cmp <- compose(identity, f)
    env <- if (is.null(environment(f))) baseenv() else environment(f)
    expect_identical(parent.env(environment(cmp)), env)
  }
})

test_that("one-sided formula of a function is lifted", {
  add <- function(a, b) a / b
  cmp <- compose(~add, list)
  vals <- {set.seed(1); runif(10)}
  for (val in vals)
    expect_equal(cmp(val, val + 1), add(val, val + 1))
})

context("Decomposing compositions")

test_that("decomposing a non-composite function wraps it unchanged", {
  for (f in fn_kinds[c("closure", "special", "builtin")])
    expect_identical(decompose(f), f)
})

test_that("error is signalled when decomposing a non-decomposable object", {
  errmsg <- function(x) {
    cls <- paste(deparse(class(x)), collapse = "")
    sprintf("Cannot decompose object of class %s", cls)
  }

  nondecomposable <- list(
    as.data.frame(1:3),
    quote(x),
    quote(function() NULL),
    structure(NA, class = "foo")
  )

  for (obj in nondecomposable)
    expect_error(decompose(obj), errmsg(obj))
})

test_that("list of composite functions is flat", {
  for (assoc in cmps)
    expect_equivalent(decompose(assoc), fs)
})

test_that("decompose() inverts compose()", {
  expect_equivalent(decompose(compose(fs)), fs)
  expect_equivalent(decompose(compose(fs[[1]], fs[[2]], fs[[3]])), fs)
  expect_equivalent(decompose(compose(fs[[1]], fs[[2]])), fs[1:2])
  expect_equivalent(decompose(compose(fs[[1]])), fs[[1]])
})

test_that("compose() inverts decompose()", {
  # Test by call
  expect_equal(compose(decompose(compose(fs))), compose(fs))

  # Test by value
  for (f in fn_kinds)
    expect_equal(compose(decompose(f)), f)
})

test_that("compose operators implicitly partialize operands that are calls", {
  inc <- function(shift, x) x + shift
  log2 <- function(x) log(x, base = 2)

  f0 <- function(x) log(abs(x) + 1, base = 2)
  fns <- list(
    abs %>>>% inc(1) %>>>% log(base = 2),
    log(base = 2) %<<<% inc(1) %<<<% abs,
    (abs %>>>% inc(1)) %>>>% log(base = 2),
    log(base = 2) %<<<% (inc(1) %<<<% abs),
    abs %>>>% (inc(1) %>>>% log(base = 2)),
    (log(base = 2) %<<<% inc(1)) %<<<% abs
  )

  set.seed(1)
  vals <- runif(10, 0, 100)
  for (f in fns) {
    for (x in vals) {
      expect_equal(f(x), f0(x))
    }
  }
})

test_that("compose-operator operands are called normally when in braces", {
  f0 <- function(x) log(abs(x) + 1, base = 2)

  boxed_log <- list(function(x) log(x, base = 2))
  fns <- list(
    {identity(abs)} %>>>% {function(x) x + 1} %>>>% log(base = 2),
    {identity(abs)} %>>>% {function(x) x + 1} %>>>% {boxed_log[[1]]},
    log(base = 2) %<<<% {function(x) x + 1} %<<<% {identity(abs)},
    {boxed_log[[1]]} %<<<% {function(x) x + 1} %<<<% {identity(abs)},
    ({identity(abs)} %>>>% {function(x) x + 1}) %>>>% log(base = 2),
    ({identity(abs)} %>>>% {function(x) x + 1}) %>>>% {boxed_log[[1]]},
    log(base = 2) %<<<% ({function(x) x + 1} %<<<% {identity(abs)}),
    {boxed_log[[1]]} %<<<% ({function(x) x + 1} %<<<% {identity(abs)}),
    {identity(abs)} %>>>% ({function(x) x + 1} %>>>% log(base = 2)),
    {identity(abs)} %>>>% ({function(x) x + 1} %>>>% {boxed_log[[1]]}),
    (log(base = 2) %<<<% {function(x) x + 1}) %<<<% {identity(abs)},
    ({boxed_log[[1]]} %<<<% {function(x) x + 1}) %<<<% {identity(abs)}
  )

  set.seed(1)
  vals <- runif(10, 0, 100)
  for (f in fns) {
    for (x in vals) {
      expect_equal(f(x), f0(x))
    }
  }
})

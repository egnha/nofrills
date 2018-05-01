make_funs <- function(n) {
  lapply(seq_len(n), function(i) {force(i); function(. = NULL) c(i, .)})
}

fs <- make_funs(3)

fn_kinds <- list(
  closure     = function(x) NULL,
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
  (!!fs[[1]]) %<<<% !!fs[[2]] %<<<% !!fs[[3]],
  (!!((!!fs[[1]]) %<<<% !!fs[[2]])) %<<<% !!fs[[3]],
  (!!fs[[1]]) %<<<% !!((!!fs[[2]]) %<<<% !!fs[[3]]),
  # Forward composition
  (!!fs[[3]]) %>>>% !!fs[[2]] %>>>% !!fs[[1]],
  (!!((!!fs[[3]]) %>>>% !!fs[[2]])) %>>>% !!fs[[1]],
  (!!fs[[3]]) %>>>% !!((!!fs[[2]]) %>>>% !!fs[[1]])
)

context("Composing functions")

test_that("composition returns a single function unchanged", {
  for (f in fn_kinds)
    expect_equal(compose(f), f)
})

test_that("empty or length 0 composition yields the identity function (#41)", {
  expect_identical(identity, compose())
  expect_identical(identity, compose(NULL))
  expect_identical(identity, compose(list()))
  expect_identical(identity, compose(!!!list()))
})

test_that("NULL and identity are dropped when composing", {
  expect_identical(log, compose(NULL, log))
  expect_identical(log, compose(log, NULL))
  expect_identical(log, compose(identity, log))
  expect_identical(log, compose(log, identity))
  expect_identical(log, NULL %>>>% log)
  expect_identical(log, log %>>>% NULL)
  expect_identical(log, identity %>>>% log)
  expect_identical(log, log %>>>% identity)
  expect_identical(log, NULL %<<<% log)
  expect_identical(log, log %<<<% NULL)
  expect_identical(log, identity %<<<% log)
  expect_identical(log, log %<<<% identity)

  inc <- function(x) x + 1
  cmp0 <- compose(log, inc)
  cmps <- list(
    compose(NULL, log, inc),
    compose(log, NULL, inc),
    compose(log, inc, NULL),
    compose(identity, log, inc),
    compose(log, identity, inc),
    compose(log, inc, identity),
    inc %>>>% log %>>>% NULL,
    inc %>>>% NULL %>>>% log,
    NULL %>>>% inc %>>>% log,
    inc %>>>% log %>>>% identity,
    inc %>>>% identity %>>>% log,
    identity %>>>% inc %>>>% log,
    NULL %<<<% log %<<<% inc,
    log %<<<% NULL %<<<% inc,
    log %<<<% inc %<<<% NULL,
    identity %<<<% log %<<<% inc,
    log %<<<% identity %<<<% inc,
    log %<<<% inc %<<<% identity
  )

  # Function equality by equality of return values and composite functions
  vals <- {set.seed(1); runif(10)}
  for (cmp in cmps) {
    expect_equal(cmp(vals), cmp0(vals))
    expect_identical(decompose(cmp), list(log, inc))
  }
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
    compose((!!gs[[1]]) %<<<% !!gs[[2]] %<<<% !!gs[[3]] %<<<% !!gs[[4]]),
    compose((!!gs[[4]]) %>>>% !!gs[[3]] %>>>% !!gs[[2]] %>>>% !!gs[[1]]),
    compose(gs[[1]], compose(gs[[2]], gs[[3]], gs[[4]])),
    compose((!!gs[[1]]) %<<<% !!compose(gs[[2]], gs[[3]], gs[[4]])),
    compose((!!compose(gs[[2]], gs[[3]], gs[[4]])) %>>>% !!gs[[1]]),
    compose(gs[[1]], compose(gs[[2]], (!!gs[[3]]) %<<<% !!gs[[4]])),
    compose(gs[[1]], compose(gs[[2]], (!!gs[[4]]) %>>>% !!gs[[3]])),
    compose(gs[[1]], compose(gs[[2]], compose(gs[[3]], gs[[4]]))),
    compose(gs[[1]], (!!gs[[2]]) %<<<% !!compose(gs[[3]], gs[[4]])),
    compose(gs[[1]], (!!compose(gs[[3]], gs[[4]])) %>>>% !!gs[[2]]),
    compose(gs[[1]], compose(gs[[2]], compose(gs[[3]], compose(gs[[4]])))),
    compose((!!gs[[1]]) %<<<% !!((!!gs[[2]]) %<<<% !!((!!gs[[3]]) %<<<% !!gs[[4]]))),
    compose((!!((!!((!!gs[[4]]) %>>>% !!gs[[3]])) %>>>% (!!gs[[2]]))) %>>>% !!gs[[1]])
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
    cmp <- compose(function(...) NULL, f)
    env <- if (is.null(environment(f))) baseenv() else environment(f)
    expect_identical(parent.env(environment(cmp)), env)
  }
})

test_that("composition operator obeys magrittr semantics (#39)", {
  # Insertion of initial '.', in case it doesn't appear among the arguments
  f0 <- function(x) {
    upper <- sprintf("%s", toupper(x[[1]]))
    paste(upper, collapse = "")
  }
  f1 <- .[[1]] %>>>% toupper %>>>% sprintf("%s", .) %>>>% paste(collapse = "")
  f2 <- .[[1]] %>>>% toupper() %>>>% sprintf("%s", .) %>>>% paste(collapse = "")
  f3 <- paste(collapse = "") %<<<% sprintf("%s", .) %<<<% toupper %<<<% .[[1]]
  f4 <- paste(collapse = "") %<<<% sprintf("%s", .) %<<<% toupper() %<<<% .[[1]]
  x <- list(letters)
  expect_identical(f0(x), paste(LETTERS, collapse = ""))
  expect_identical(f1(x), f0(x))
  expect_identical(f2(x), f0(x))
  expect_identical(f3(x), f0(x))
  expect_identical(f4(x), f0(x))

  # Anonymous function of '.' using {...}
  f0 <- function(x) log(abs(x) + 1)
  f1 <- abs %>>>% {. + 1} %>>>% log
  f2 <- log %<<<% {. + 1} %<<<% abs
  vals <- {set.seed(1); runif(10, -1, 1)}
  for (val in vals) {
    expect_equal(f1(val), f0(val))
    expect_equal(f2(val), f0(val))
  }

  f0 <- function(x) {
    out <- list(result = x)
    paste(out$result, collapse = "")
  }
  f1 <- {list(result = .)} %>>>% {paste(.$result, collapse = "")}
  f2 <- {paste(.$result, collapse = "")} %<<<% {list(result = .)}
  expect_identical(f0(letters), paste(letters, collapse = ""))
  expect_identical(f1(letters), f0(letters))
  expect_identical(f2(letters), f0(letters))
})

test_that("composition operator operands can be unquoted", {
  f <- list(log)
  inc <- 1
  f0 <- function(x) log(abs(x) + 1)
  f1 <- abs %>>>% {. + !!inc} %>>>% (!!f[[1]])
  f2 <- (!!f[[1]]) %<<<% {. + !!inc} %<<<% abs
  f3 <- (!!(abs %>>>% {. + !!inc})) %>>>% (!!f[[1]])
  f4 <- (!!f[[1]]) %<<<% (!!({. + !!inc} %<<<% abs))
  vals <- {set.seed(1); runif(10, -1, 1)}
  for (val in vals) {
    expect_equal(f1(val), f0(val))
    expect_equal(f2(val), f0(val))
    expect_equal(f3(val), f0(val))
    expect_equal(f4(val), f0(val))
  }
})

test_that("composition operator yields flattened compositions", {
  sq <- function(x) x^2
  id <- function(x) x

  p <- sq %>>>% sq
  q <- p %>>>% id %>>>% sq
  r <- q %>>>% id
  expect_identical(decompose(r), list(id, sq, id, sq, sq))

  p <- sq %<<<% sq
  q <- sq %<<<% id %<<<% p
  r <- id %<<<% q
  expect_identical(decompose(r), list(id, sq, id, sq, sq))
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

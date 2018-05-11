make_funs <- function(n) {
  lapply(seq_len(n), function(i) {
    force(i)
    function(. = NULL) c(i, .)
  })
}

fs <- make_funs(3)

fn_kinds <- list(
  closure     = function(x) NULL,
  special     = log,
  builtin     = c,
  composition = compose(fs)
)

cmp <- function() {
  fs[[3]](fs[[2]](fs[[1]]()))
}

cmps <- list(
  compose(fs[[1]], compose(fs[[2]], compose(fs[[3]]))),
  compose(compose(fs[[1]], compose(fs[[2]])), fs[[3]]),
  compose(fs[[1]], compose(fs[[2]], fs[[3]])),
  compose(compose(fs[[1]], fs[[2]]), fs[[3]]),
  compose(compose(fs[[1]], fs[[2]], fs[[3]])),
  compose(fs[[1]], fs[[2]], fs[[3]]),
  (!!fs[[1]]) %>>>% !!fs[[2]] %>>>% !!fs[[3]],
  (!!((!!fs[[1]]) %>>>% !!fs[[2]])) %>>>% !!fs[[3]],
  (!!fs[[1]]) %>>>% !!((!!fs[[2]]) %>>>% !!fs[[3]])
)

context("Composing functions")

test_that("empty or NULL composition yields NULL", {
  expect_null(compose())
  expect_null(compose(NULL))
  expect_null(compose(list()))
  expect_null(compose(!!!list()))
})

test_that("NULL is dropped when composing", {
  expect_equal(compose(log), compose(NULL, log))
  expect_equal(compose(log), compose(log, NULL))
  expect_equal(compose(log), NULL %>>>% log)
  expect_equal(compose(log), log %>>>% NULL)

  inc <- function(x) x + 1
  cmp0 <- compose(inc, log)
  cmps <- list(
    compose(NULL, inc, log),
    compose(inc, NULL, log),
    compose(inc, log, NULL),
    inc %>>>% log %>>>% NULL,
    inc %>>>% NULL %>>>% log,
    NULL %>>>% inc %>>>% log
  )

  # Function equality by equality of return values and composite functions
  vals <- {set.seed(1); runif(10)}
  for (cmp in cmps) {
    expect_equal(cmp(vals), cmp0(vals))
    expect_equivalent(as.list(cmp), list(inc, log))
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
  expect_identical(value, 3:1)
  for (assoc in cmps)
    expect_identical(assoc(), value)
})

test_that("nested compositions are flattened", {
  gs <- make_funs(4)

  # Test by call
  cmps <- list(
    compose(gs[[1]], gs[[2]], gs[[3]], gs[[4]]),
    compose(compose(gs[[1]], gs[[2]], gs[[3]], gs[[4]])),
    compose((!!gs[[1]]) %>>>% !!gs[[2]] %>>>% !!gs[[3]] %>>>% !!gs[[4]]),
    compose(gs[[1]], compose(gs[[2]], gs[[3]], gs[[4]])),
    compose((!!gs[[1]]) %>>>% !!compose(gs[[2]], gs[[3]], gs[[4]])),
    compose((!!compose(gs[[1]], gs[[2]], gs[[3]])) %>>>% !!gs[[4]]),
    compose(gs[[1]], compose(gs[[2]], (!!gs[[3]]) %>>>% !!gs[[4]])),
    compose(gs[[1]], compose(gs[[2]], compose(gs[[3]], gs[[4]]))),
    compose(gs[[1]], (!!gs[[2]]) %>>>% !!compose(gs[[3]], gs[[4]])),
    compose(gs[[1]], (!!compose(gs[[3]], gs[[3]])) %>>>% !!gs[[4]]),
    compose(gs[[1]], compose(gs[[2]], compose(gs[[3]], compose(gs[[4]])))),
    compose((!!gs[[1]]) %>>>% !!((!!gs[[2]]) %>>>% !!((!!gs[[3]]) %>>>% !!gs[[4]]))),
    compose((!!((!!((!!gs[[1]]) %>>>% !!gs[[2]])) %>>>% (!!gs[[3]]))) %>>>% !!gs[[4]])
  )
  for (cmp in cmps)
    expect_equivalent(as.list(cmp), gs)

  # Test by value
  cmps <- Reduce(compose, gs, accumulate = TRUE)
  expect_equivalent(cmps[[1]], gs[[1]])
  for (i in seq_along(gs)[-1])
    expect_equivalent(as.list(cmps[[i]]), gs[seq_len(i)])
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
    special = `[[`,
    builtin = `+`
  )
  for (inner in fs) {
    fmls_inner <- formals(args(inner) %||% rlang::as_closure(inner))
    expect_identical(formals(compose(inner, outer)), fmls_inner)
  }
})

test_that("environment of composition is child of inner-function environment", {
  fs <- c(
    fn_kinds[names(fn_kinds) != "composition"],
    local(function() NULL)
  )
  for (f in fs) {
    cmp <- compose(f, function(...) NULL)
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
  f1 <- {.[[1]]} %>>>% toupper %>>>% sprintf("%s", .) %>>>% paste(collapse = "")
  f2 <- {.[[1]]} %>>>% toupper() %>>>% sprintf("%s", .) %>>>% paste(collapse = "")

  x <- list(letters)
  expect_identical(f0(x), paste(LETTERS, collapse = ""))
  expect_identical(f1(x), f0(x))
  expect_identical(f2(x), f0(x))

  x <- mtcars
  expect_identical(f0(x), paste(mtcars[[1]], collapse = ""))
  expect_identical(f1(x), f0(x))
  expect_identical(f2(x), f0(x))

  # Anonymous function of '.' using {...}
  f0 <- function(x) log(abs(x) + 1)
  f1 <- abs %>>>% {. + 1} %>>>% log
  vals <- {set.seed(1); runif(10, -1, 1)}
  for (val in vals)
    expect_equal(f1(val), f0(val))

  f0 <- function(x) {
    out <- list(result = x)
    paste(out$result, collapse = "")
  }
  f1 <- {list(result = .)} %>>>% {paste(.$result, collapse = "")}
  expect_identical(f0(letters), paste(letters, collapse = ""))
  expect_identical(f1(letters), f0(letters))
})

test_that("composition operator operands can be unquoted", {
  f <- list(log)
  inc <- 1
  f0 <- function(x) log(abs(x) + 1)
  f1 <- abs %>>>% {. + !!inc} %>>>% !!f[[1]]
  f2 <- (!!(abs %>>>% {. + !!inc})) %>>>% !!f[[1]]
  vals <- {set.seed(1); runif(10, -1, 1)}
  for (val in vals) {
    expect_equal(f1(val), f0(val))
    expect_equal(f2(val), f0(val))
  }
})

test_that("composition operator yields flattened compositions", {
  sq <- function(x) x^2
  id <- function(x) x

  p <- sq %>>>% sq
  q <- p %>>>% id %>>>% sq
  r <- q %>>>% id
  expect_equivalent(as.list(r), list(sq, sq, id, sq, id))
})

test_that("in pipeline, void call is interpreted as its caller", {
  id <- function(f) f
  cmps <- list(
    abs %>>>% log(),
    abs %>>>% id(log)()
  )
  for (cmp in cmps)
    expect_identical(log, as.list(cmp)[[2]])
})

test_that("error is signaled if void call in pipeline doesn't yield function", {
  foo <- quote(foo)

  expect_errors_with_message(
    "object 'foo' of mode 'function' was not found",
    abs %>>>% foo(),
    abs %>>>% identity(foo)()
  )
})

test_that("namespace operators are literally interpreted", {
  f <- stats::runif %>>>% base:::log(base = 2)
  expect_equal(
    {set.seed(1); f(10)},
    {set.seed(1); log(runif(10), base = 2)}
  )
})

test_that("parentheses are literally interpreted", {
  . <- seq_len
  f <- identity(.) %>>>% (identity(.)) %>>>% (log(2) %>>>% sum)
  expect_equal(
    f(10),
    sum(log(seq_len(10), 2))
  )
})

test_that("subsetters are literally interpreted (#51)", {
  fs <- list(log, sum, exp, sq = function(x) x^2)
  vals <- {set.seed(1); runif(10, 1, 2)}

  f <- fs[[1]] %>>>% fs[2:3] %>>>% fs$sq
  expect_equal(f(vals), exp(sum(log(vals)))^2)
})

test_that("functions in composition can be named", {
  f0 <- function(x) log(abs(x) + 1)

  nm <- "logarithm"
  fs <- list(
    compose(abs, inc = function(x) x + 1, !!nm := log),
    abs %>>>% inc: {. + 1} %>>>% !!nm: log
  )

  vals <- {set.seed(1); runif(10)}

  for (f in fs) {
    expect_equal(f(vals), f0(vals))

    pipeline <- as.list(f)
    expect_identical(names(pipeline), c("", "inc", "logarithm"))
    expect_equal(pipeline[[1]](vals), abs(vals))
    expect_equal(pipeline$logarithm(vals), log(vals))
    expect_equal(pipeline$inc(vals), vals + 1)
  }
})

test_that("error is signaled when implicit partialization is invalid (#43)", {
  f <- function(x, y) NULL
  notfun <- quote(notfun)

  expect_error(
    identity %>>>% f(a, b),
    "`f\\(\\., a, b\\)` is an invalid call"
  )
  expect_error(
    identity %>>>% f(z = .),
    "`f\\(z = \\.\\)` is an invalid call"
  )
  expect_error(
    identity %>>>% notfun(.),
    "object 'notfun' of mode 'function' was not found"
  )
})

test_that("distilling a composition drops identity components", {
  cmps <- list(
    log %>>>% sum: sum,
    identity %>>>% log %>>>% sum: sum,
    log %>>>% identity %>>>% sum: sum,
    log %>>>% sum: sum %>>>% identity,
    identity %>>>% identity %>>>% log %>>>% sum: sum,
    identity %>>>% log %>>>% identity %>>>% sum: sum,
    identity %>>>% log %>>>% sum: sum %>>>% identity,
    log %>>>% identity %>>>% identity %>>>% sum: sum,
    log %>>>% identity %>>>% sum: sum %>>>% identity,
    log %>>>% sum: sum %>>>% identity %>>>% identity
  )

  vals <- {set.seed(1); runif(10, 1, 2)}
  out <- sum(log(vals))

  for (cmp in cmps) {
    dist <- distill(cmp)
    expect_true(inherits(dist, "CompositeFunction"))
    expect_length(dist, 2)
    expect_named(dist, c("", "sum"))
    expect_identical(dist[[1]], log)
    expect_identical(dist[[2]], sum)
    expect_equal(dist(vals), out)
  }
})

test_that("pipeline of identity function distills to identity function", {
  cmps <- list(
    identity,
    identity %>>>% identity,
    identity %>>>% identity %>>>% identity
  )

  for (cmp in cmps) {
    dist <- distill(cmp)
    if (inherits(cmp, "CompositeFunction")) {
      expect_true(inherits(dist, "CompositeFunction"))
      expect_length(dist, 1)
      expect_identical(dist[[1]], identity)
    } else {
      expect_identical(dist, identity)
    }
  }
})

test_that("distilled non-composite function is itself", {
  for (f in fn_kinds[names(fn_kinds) != "composition"])
    expect_identical(distill(f), f)
})

context("Decomposing compositions")

test_that("list of composite functions is flat", {
  for (assoc in cmps)
    expect_equivalent(as.list(assoc), fs)
})

test_that("as.list() inverts compose()", {
  expect_equivalent(as.list(compose(fs)), fs)
  expect_equivalent(as.list(compose(fs[[1]], fs[[2]], fs[[3]])), fs)
  expect_equivalent(as.list(compose(fs[[1]], fs[[2]])), fs[1:2])
})

test_that("compose() inverts as.list()", {
  cmp1 <- compose(fs)
  cmp2 <- compose(as.list(compose(fs)))

  # Test by call
  expect_equal(cmp1, cmp2)

  # Test by value
  vals <- {set.seed(1); runif(10)}
  for (val in vals)
    expect_equal(cmp1(val), cmp2(val))
})

context("Generic methods")

sq <- function(x) x^2
foo <- sq %>>>% inc:{. + 1} %>>>% log:log

vals <- {set.seed(1); runif(10, 1, 2)}

test_that("function in composition can be extracted by name", {
  expect_equal(foo$inc(vals), vals + 1)
  expect_equal(foo[["inc"]](vals), vals + 1)
  expect_equal(foo$log(vals), log(vals))
  expect_equal(foo[["log"]](vals), log(vals))
})

test_that("compositions can be filtered by name", {
  expect_equal(foo["inc"](vals), vals + 1)
  expect_equal(foo["log"](vals), log(vals))
  expect_equal(foo[c("inc", "log")](vals), log(vals + 1))
  expect_equal(foo[c("log", "inc")](vals), log(vals) + 1)
})

test_that("compositions can be filtered by position", {
  expect_equal(foo[1](vals), sq(vals))
  expect_equal(foo[2](vals), vals + 1)
  expect_equal(foo[3](vals), log(vals))
  expect_equal(foo[-1](vals), log(vals + 1))
  expect_equal(foo[-2](vals), log(sq(vals)))
  expect_equal(foo[-3](vals), sq(vals) + 1)
  expect_equal(foo[c(1, 2)](vals), sq(vals) + 1)
  expect_equal(foo[c(2, 1)](vals), sq(vals + 1))
  expect_equal(foo[c(1, 3)](vals), log(sq(vals)))
  expect_equal(foo[c(3, 1)](vals), sq(log(vals)))
  expect_equal(foo[c(2, 3)](vals), log(vals + 1))
  expect_equal(foo[c(3, 2)](vals), log(vals) + 1)
  expect_equal(foo[-c(1, 2)](vals), log(vals))
  expect_equal(foo[-c(1, 3)](vals), vals + 1)
  expect_equal(foo[-c(2, 3)](vals), sq(vals))
  expect_equal(foo[c(1, 2, 3)](vals), log(sq(vals) + 1))
  expect_equal(foo[c(1, 3, 2)](vals), log(sq(vals)) + 1)
  expect_equal(foo[c(2, 3, 1)](vals), sq(log(vals + 1)))
  expect_equal(foo[c(2, 1, 3)](vals), log(sq(vals + 1)))
  expect_equal(foo[c(3, 1, 2)](vals), sq(log(vals)) + 1)
  expect_equal(foo[c(3, 2, 1)](vals), sq(log(vals) + 1))
})

test_that("compositions can be filtered by predicate", {
  expect_null(foo[c(F, F, F)])
  expect_equal(foo[c(F, F, T)](vals), log(vals))
  expect_equal(foo[c(F, T, F)](vals), vals + 1)
  expect_equal(foo[c(F, T, T)](vals), log(vals + 1))
  expect_equal(foo[c(T, F, F)](vals), sq(vals))
  expect_equal(foo[c(T, F, T)](vals), log(sq(vals)))
  expect_equal(foo[c(T, T, F)](vals), sq(vals) + 1)
  expect_equal(foo[c(T, T, T)](vals), log(sq(vals) + 1))
})

test_that("error signaled when predicate is of unequal length", {
  expect_error(
    foo[c(T, T, T, T)],
    "Length of predicate \\(4\\) must equal length of composition \\(3\\)"
  )
})

test_that("filtering composition out-of-bounds yields NULL", {
  expect_null(foo[0])
  expect_null(foo[length(foo) + 1])
  expect_null(foo$nonexistent)
  expect_null(foo[["nonexistent"]])
})

test_that("compositions can be replaced by name", {
  bar <- sq %>>>% inc:{. + 1} %>>>% log:log

  bar$log <- NULL
  expect_equal(bar(vals), sq(vals) + 1)

  bar$inc <- sin
  expect_equal(bar(vals), sin(sq(vals)))

  bar[["inc"]] <- cos
  expect_equal(bar(vals), cos(sq(vals)))

  bar[["inc"]] <- NULL
  expect_equal(bar(vals), sq(vals))
})

test_that("compositions can be replaced by index", {
  bar <- sq %>>>% inc:{. + 1} %>>>% log:log

  bar[[3]] <- NULL
  expect_equal(bar(vals), sq(vals) + 1)

  bar[[2]] <- sin
  expect_equal(bar(vals), sin(sq(vals)))
})

test_that("composition names are in call-order", {
  expect_named(foo, c("", "inc", "log"))
})

test_that("unnamed compositions have empty-string names", {
  expect_named(abs %>>>% log %>>>% sin, c("", "", ""))
})

test_that("compositions can be renamed", {
  bar <- sq %>>>% inc:{. + 1} %>>>% log:log

  names(bar) <- c("square", "increment", "logarithm")
  expect_named(bar, c("square", "increment", "logarithm"))
  expect_equal(bar$square(vals), sq(vals))
  expect_equal(bar$increment(vals), vals + 1)
  expect_equal(bar$logarithm(vals), log(vals))

  names(bar) <- NULL
  expect_named(bar, rep("", length(bar)))
})

test_that("composition length is the number of component functions", {
  expect_length(compose(sin), 1)
  expect_length(sin %>>>% cos, 2)
  expect_length(sin %>>>% cos %>>>% tan, 3)
})

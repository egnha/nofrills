expectations <- function(expect) {
  expect <- substitute(expect)
  function(target, ...) {
    exprs <- eval(substitute(alist(...)))
    for (expr in exprs) {
      expectation <- bquote(.(expect)(.(expr), .(target)))
      eval.parent(expectation)
    }
  }
}

expect_errors_with_message <- expectations(expect_error)

expect_all_equal <- expectations(expect_equal)

expect_dots <- function(...) {
  fs <- eval(substitute(alist(...)))
  for (f in fs) {
    expectation <- bquote(expect_true(hasName(formals(.(f)), "...")))
    eval.parent(expectation)
  }
}

# As of testthat 1.0.2.9000, objects are captured as quosures by `expect_*()`.
# This causes spurious test failures, due to a bug in `rlang::enquo()`
# (https://github.com/tidyverse/rlang/issues/280) and its inability to
# comprehend literal unquoting operators
# (cf. https://github.com/tidyverse/rlang/issues/264).

expect_equal_ <- function(object, expected, ...) {
  force(object)
  force(expected)
  expect_equal(object, expected, ...)
}

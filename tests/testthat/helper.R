expect_errors_with_message <- function(regexp, ...) {
  exprs <- eval(substitute(alist(...)))
  expectations <- lapply(exprs, function(expr)
    bquote(expect_error(.(expr), .(regexp)))
  )
  for (expectation in expectations)
    eval.parent(expectation)
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

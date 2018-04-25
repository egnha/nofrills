#' Raw quotation of expressions
#'
#' `exprs_()` is an extension of [rlang::exprs()] that comprehends literal
#' unquoting operators: `QUQ()`, `QUQS()` are substituted as `` `!!`() ``,
#' `` `!!!`() ``, resp.
#'
#' @param ... Unevaluated expressions to capture.
#' @return List of expressions.
#'
#' @noRd
exprs_ <- function(...) {
  lapply(quos(...), quo_get_expr_)
}

quo_get_expr_ <- local({
  quq <- list(
    QUQ  = as.name("!!"),
    QUQS = as.name("!!!")
  )

  function(x) {
    do.call("substitute", list(quo_get_expr(x), quq))
  }
})

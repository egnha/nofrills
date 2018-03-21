#' Raw quotation of expressions
#'
#' `exprs_()` is an extension of [rlang::exprs()] that comprehends literal
#' unquoting operators: `QUQ()`, `QUQS()`, corresponding to `!!` and `!!!` resp.
#'
#' @param ... Expressions to capture, unevaluated.
#' @return List of expressions.
#'
#' @noRd
exprs_ <- function(...)
  lapply(quos(...), quo_get_expr_)

quo_get_expr_ <- local({
  quote_uq <- list(QUQ = as.name("!!"), QUQS = as.name("!!!"))
  function(x)
    do.call("substitute", list(quo_get_expr(x), quote_uq))
})

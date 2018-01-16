#' Raw quotation of expressions
#'
#' `exprs_()` is an extension of [rlang::exprs()] that comprehends literal
#' unquoting operators: `QUQ()`, `QUQS()`, corresponding to `!!` and `!!!` resp.
#'
#' @param ...,.ignore_empty Same as for [rlang::exprs()].
#' @return List of expressions.
#'
#' @noRd
exprs_ <- local({
  quote_uq <- list(QUQ = as.name("!!"), QUQS = as.name("!!!"))
  f_rhs_ <- function(x)
    do.call("substitute", list(f_rhs(x), quote_uq))

  function(..., .ignore_empty = "trailing")
    lapply(quos(..., .ignore_empty = .ignore_empty), f_rhs_)
})

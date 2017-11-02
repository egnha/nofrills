#' Raw quotation of expressions
#'
#' `exprs_()` is an extension of [rlang::exprs()] that comprehends literal
#' unquoting operators: `QUQ()`, `QUQS()`, `QUQE()`.
#'
#' @param ...,.ignore_empty Same as for [rlang::exprs()].
#' @return List of expressions.
#'
#' @noRd
exprs_ <- function(..., .ignore_empty = "trailing") {
  lapply(quos(..., .ignore_empty = .ignore_empty), f_rhs_)
}

f_rhs_ <- local({
  quote_uq <- lapply(set_names(c("UQ", "UQS", "UQE")), as.name)
  names(quote_uq) <- paste0("Q", names(quote_uq))
  function(x)
    do.call("substitute", list(f_rhs(x), quote_uq))
})

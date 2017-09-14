exprs_ <- function(..., .ignore_empty = "trailing") {
  lapply(quos(..., .ignore_empty = .ignore_empty), f_rhs_)
}

f_rhs_ <- function(x) {
  eval_bare(substitute(substitute(., quote_uq), list(. = f_rhs(x))))
}

quote_uq <- lapply(set_names(c("UQ", "UQS", "UQE")), as.name)
names(quote_uq) <- paste0("Q", names(quote_uq))

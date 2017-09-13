exprs_ <- function(..., .ignore_empty = "trailing") {
  lapply(quos(..., .ignore_empty = .ignore_empty), f_rhs_)
}

f_rhs_ <- function(x) {
  eval_bare(substitute(substitute(., quq), list(. = f_rhs(x))))
}

quq <- lapply(set_names(c("UQ", "UQS", "UQE")), as.name)
names(quq) <- paste0("Q", names(quq))

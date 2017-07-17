#' Low-cost anonymous function
#'
#' `eff()` enables you concisely create \dQuote{anonymous} functions of
#' arbitrary signature.
#'
#' @param ... Function declaration.
#'
#' @examples
#' f <- eff(x, y := x + y)
#' f(1, 2)
#'
#' f <- eff(x, y = 1 := x + y)
#' f(1)
#'
#' f <- eff(x, y = 1, ... := log(x + y, ...))
#' f(1, 1, base = 2)
#'
#' f <- eff(x, ... = , y := log(x + y, ...))
#' f(1, base = 2, y = 1)
#'
#' zero <- 0
#' is_positive <- eff(x := x > !! zero)
#' is_positive(1)
#' is_positive(-1)
#'
#' @export
#' @importFrom rlang dots_definitions
eff <- function(...) {
  dd <- dots_definitions(...)
  declare_function(dd, parent.frame())
}

#' @importFrom rlang abort new_function quo_expr
declare_function <- function(dd, env) {
  if (length(dd$defs) != 1L)
    abort("Invalid function declaration")
  args <- extract_args(dd)
  body <- quo_expr(dd$defs[[1]]$rhs)
  new_function(args, body, env)
}

#' @importFrom rlang f_rhs quo quo_name
extract_args <- function(dd) {
  args <- c(dd$dots, extract_final_arg(dd$defs))
  not_named <- !nzchar(names(args))
  names(args)[not_named] <- vapply(args[not_named], quo_name, character(1))
  args[not_named] <- list(quo())
  lapply(args, f_rhs)
}

extract_final_arg <- function(def) {
  nm <- names(def)
  if (nzchar(nm))
    `names<-`(list(def[[1]]$lhs), nm)
  else
    def[[1]]$lhs
}

#' @rdname eff
#' @export
#' @importFrom rlang enquo eval_bare UQS
as_eff <- function(x) {
  if (is.function(x))
    return(x)
  q <- enquo(x)
  if (!is_eff_expr(q))
    abort("Ill-formed function expression")
  dots <- eval_bare(`[[<-`(f_rhs(q), 1, quote(alist)))
  eff(UQS(dots))
}

is_eff_expr <- function(q) {
  rhs <- f_rhs(q)
  is.call(rhs) && identical(rhs[[1]], as.name("."))
}

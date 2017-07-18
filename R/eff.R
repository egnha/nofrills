#' Low-cost anonymous functions
#'
#' `eff()` enables you to concisely create (anonymous) functions of arbitrary
#' call signature.
#'
#' @param ... Function declaration (see below).
#' @return `eff()` returns a function whose enclosing environment is the calling
#'   environment of `eff()`.
#'
#' @section Function declarations: A _function declaration_ is a concise
#'   expression that specifies a function’s arguments and body. It is a
#'   comma-separated expression of the form
#'   \preformatted{    arg1, arg2, ..., argN ~ body}
#'   The part on the left of `~` corresponds to what you would write in a
#'   conventional function declaration `function(...)`: each of `arg1`, `arg2`,
#'   \dots, `argN` is either a bare argument (e.g., `x` or `...`) or an argument
#'   with default value (e.g., `x = 1`). The part on the right of `~` is the
#'   function body, i.e., an expression of the arguments.
#'
#' @examples
#' f <- eff(x ~ x + 1)
#' f(1)
#'
#' f <- eff(x, y ~ x + y)
#' f(1, 2)
#'
#' f <- eff(x, y = 2 ~ x + y)
#' f(1)
#'
#' f <- eff(x, y = 1, ... ~ log(x + y, ...))
#' f(1, 1, base = 2)
#'
#' # to specify '...' in the middle of the call signature, write '... = '
#' f <- eff(x, ... = , y ~ log(x + y, ...))
#' f(1, base = 2, y = 1)
#'
#' # unquoting of names is supported
#' zero <- 0
#' is_positive <- eff(x ~ x > !! zero)
#' is_positive(1)
#' is_positive(-1)
#'
#' @importFrom rlang abort exprs is_empty new_function
#' @export
#'
eff <- function(...) {
  xs <- exprs(...)
  if (is_empty(xs))
    abort("No function body specified")
  else {
    dec <- get_fn_declaration(xs)
    new_function(dec$args, dec$body, parent.frame())
  }
}

get_fn_declaration <- function(xs) {
  n <- length(xs)
  args <- get_args(xs[-n])
  remains <- behead(xs[n])
  list(args = c(args, remains$head), body = remains$body)
}

#' @importFrom rlang expr_name is_empty
get_args <- function(xs) {
  if (is_empty(xs))
    return(NULL)
  no_name <- !nzchar(names(xs))
  names(xs)[no_name] <- vapply(xs[no_name], expr_name, character(1))
  xs[no_name] <- .EMPTY_SYMBOL
  xs
}

#' @importFrom rlang f_rhs
behead <- function(x) {
  list(head = get_head(x), body = f_rhs(x[[1]]))
}

#' @importFrom rlang f_lhs
get_head <- function(x) {
  nm <- names(x)
  lhs <- f_lhs(x[[1]])
  if (nzchar(nm))
    `names<-`(list(lhs), nm)
  else
    `names<-`(.EMPTY_SYMBOL, expr_name(lhs))
}

.EMPTY_SYMBOL <- list(quote(expr =))

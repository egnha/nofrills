#' Low-cost anonymous functions
#'
#' `eff()` enables you to concisely create (anonymous) functions of arbitrary
#' call signature. For even less visual noise, `..()` is provided as an alias
#' of `eff()`.
#'
#' @param ... Function declaration (see below). rlang’s
#'   [quasiquotation][rlang::quasiquotation] syntax is supported.
#' @param ..env Environment in which to create the function (i.e., the
#'   function’s [enclosing environment][base::environment]).
#'
#' @return A function whose enclosing environment is `..env` (by default, the
#'   calling environment of `eff()`).
#'
#' @section Function declarations: A _function declaration_ is a concise
#'   expression that specifies a function’s arguments and body. It is a
#'   comma-separated expression of the form
#'   ```
#'       arg1, arg2, ..., argN ~ body
#'   ```
#'   or
#'   ```
#'       arg1, arg2, ..., argN, ~ body
#'   ```
#'   (Note the final, separating comma in the second case.)
#'
#'   To the left of `~`, you write a conventional function-argument declaration,
#'   as you’d do in `function(...)`: each of `arg1`, `arg2`, \dots, `argN` is
#'   either a bare argument (e.g., `x` or `...`) or an argument with default
#'   value (e.g., `x = 1`). To the right of `~`, you write the function body,
#'   i.e., an expression of the arguments.
#'
#'   \subsection{Quasiquotation in function declarations}{
#'     All parts of the function declaration support rlang’s
#'     [quasiquotation][rlang::quasiquotation] syntax.
#'   }
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
#' # use one-sided formula for constant functions or commands
#' eff(~ NULL)
#' eff(~ message("!"))
#'
#' # unquoting (via `!!` or UQ()) is supported
#' zero <- 0
#' eff(x = UQ(zero) ~ x > !! zero)
#'
#' # formals and function bodies can also be spliced in
#' f <- function(x, y, ...) x + y
#' g <- function(x, y) x - y
#' frankenstein <- eff(!!! formals(f), ~ !! body(g))
#' stopifnot(identical(frankenstein, function(x, y, ...) x - y))
#'
#' @importFrom rlang abort new_function
#' @export
eff <- function(..., ..env = parent.frame()) {
  if (!is.environment(..env))
    abort("'..env' must be an environment")
  d <- get_fn_declaration(...)
  new_function(d$args, d$body, ..env)
}
#' @rdname eff
#' @export
.. <- eff

get_fn_declaration <- function(...) {
  xs <- get_exprs(...)
  args <- get_args(xs$front)
  remains <- behead(xs$back)
  list(args = c(args, remains$head), body = remains$body)
}

#' @importFrom rlang exprs
get_exprs <- function(...) {
  xs <- validate(exprs(...))
  n <- length(xs)
  list(front = xs[-n], back = xs[n])
}
#' @importFrom rlang is_empty is_formula
validate <- function(xs, n) {
  if (is_empty(xs))
    abort("No function specified")
  n <- length(xs)
  is_fml <- vapply(xs, is_formula, logical(1))
  if (any(is_fml[-n]))
    abort("Only the body (which comes last) is a formula, not the arguments")
  if (!is_fml[n])
    abort("Final argument must be a formula (specifying the body)")
  xs
}

#' @importFrom rlang expr_name is_empty
get_args <- function(xs) {
  if (is_empty(xs))
    return(NULL)
  no_name <- !nzchar(names(xs))
  names(xs)[no_name] <- vapply(xs[no_name], expr_name, character(1))
  xs[no_name] <- .BLANK
  xs
}

#' @importFrom rlang f_rhs is_formula
behead <- function(x) {
  list(head = get_head(x), body = f_rhs(x[[1]]))
}
#' @importFrom rlang f_lhs
get_head <- function(x) {
  nm <- names(x)
  arg <- f_lhs(x[[1]])
  if (is_onesided(x[[1]]))
    get_empty_head(nm)
  else
    get_nonempty_head(arg, nm)
}
is_onesided <- function(x) {
  length(x) == 2
}
get_empty_head <- function(nm) {
  if (nzchar(nm))
    abort("Default value of final argument is missing")
  NULL
}
get_nonempty_head <- function(arg, nm) {
  if (nzchar(nm))
    `names<-`(list(arg), nm)
  else
    `names<-`(.BLANK, expr_name(arg))
}

.BLANK <- list(quote(expr =))

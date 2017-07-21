#' Low-cost anonymous functions
#'
#' `eff()` enables you to create (anonymous) functions, of arbitrary call
#' signature. It comes at a lower cost than `function(<arguments>) <body>`, in
#' the sense that:
#' \itemize{
#'   \item It is shorter: `eff(x, y = 1 ~ x + y)` is equivalent to
#'     `function(x, y = 1) x + y`.
#'   \item It is safer: by enabling [quasiquotation][rlang::quasiquotation],
#'     `eff()` allows you to \dQuote{burn in} values, which guards your
#'     function from being affected by unexpected scope changes (see
#'     _Examples_).
#' }
#' For even less visual noise, `..()` is provided as an alias of `eff()`.
#'
#' @param ... Function declaration, which supports rlang’s
#'   [quasiquotation][rlang::quasiquotation] syntax.
#' @param ..env Environment in which to create the function (i.e., the
#'   function’s [enclosing environment][base::environment]).
#'
#' @return A function whose enclosing environment is `..env`.
#'
#' @section Function declarations: A _function declaration_ is an expression
#'   that specifies a function’s arguments and body, as a comma-separated
#'   expression of the form
#'   ```
#'       arg1, arg2, ..., argN ~ body
#'   ```
#'   or
#'   ```
#'       arg1, arg2, ..., argN, ~ body
#'   ```
#'   (Note in the second form that the body is a one-sided formula. This
#'   distinction is relevant for argument [splicing][rlang::UQS()]; see below.)
#'
#'   To the left of `~`, you write a conventional function-argument declaration,
#'   just as in `function(<arguments>)`: each of `arg1`, `arg2`, \dots, `argN`
#'   is either a bare argument (e.g., `x` or `...`) or an argument with default
#'   value (e.g., `x = 1`). To the right of `~`, you write the function body,
#'   i.e., an expression of the arguments.
#'
#'   \subsection{Quasiquotation}{
#'     All parts of the function declaration support rlang’s
#'     [quasiquotation][rlang::quasiquotation] syntax:
#'     \itemize{
#'       \item To unquote values (of arguments or parts of the body), use `!!`
#'         or `UQ()`:
#'         \preformatted{
#'     z <- 0
#'     eff(x, y = !! z ~ x + y)
#'     eff(x ~ x > !! z)}
#'       \item To unquote argument names (with default value), use `:=`
#'         (definition operator):
#'         \preformatted{
#'     arg <- "y"
#'     eff(x, !! arg := 0 ~ x + !! as.name(arg))}
#'       \item To splice in a (formal) list of arguments, use `!!!` or `UQS()`:
#'         \preformatted{
#'     eff(!!! alist(x, y = 0), ~ x + y)}
#'         (Note that the body in this case must be given as a one-sided
#'         formula.)
#'     }
#'   }
#'
#' @examples
#' eff(x ~ x + 1)
#'
#' eff(x, y ~ x + y)
#'
#' eff(x, y = 2 ~ x + y)
#'
#' eff(x, y = 1, ... ~ log(x + y, ...))
#'
#' ## to specify '...' in the middle, write '... = '
#' eff(x, ... = , y ~ log(x + y, ...))
#'
#' ## use one-sided formula for constant functions or commands
#' eff(~ NA)
#' eff(~ message("!"))
#'
#' ## unquoting is supported (using `!!` or UQ() from rlang)
#' zero <- 0
#' eff(x = UQ(zero) ~ x > !! zero)
#'
#' ## formals and function bodies can also be spliced in
#' f <- function(x, y) x + y
#' g <- function(y, x, ...) x - y
#' frankenstein <- eff(!!! formals(f), ~ !! body(g))
#' stopifnot(identical(frankenstein, function(x, y) x - y))
#'
#' ## unquoting protects against changes in a function’s scope
#' x <- "x"
#' f <- function() x
#' f_solid <- eff(~ !! x)
#' # both return the same value of x
#' f()
#' f_solid()
#' # but if the binding `x` is (unwittingly) changed, f() changes ...
#' x <- sin
#' f()
#' # ... while f_solid() remains unaffected
#' f_solid()
#'
#' @export
eff <- function(..., ..env = parent.frame()) {
  if (!is.environment(..env))
    rlang::abort("'..env' must be an environment")
  d <- get_fn_declaration(...)
  rlang::new_function(d$args, d$body, ..env)
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

get_exprs <- function(...) {
  xs <- validate(rlang::exprs(...))
  n <- length(xs)
  list(front = xs[-n], back = xs[n])
}
validate <- function(xs, n) {
  if (rlang::is_empty(xs))
    rlang::abort("No function specified")
  n <- length(xs)
  is_fml <- vapply(xs, rlang::is_formula, logical(1))
  if (any(is_fml[-n]))
    rlang::abort("Only the body (as last argument) should be a formula")
  if (!is_fml[n])
    rlang::abort("Final argument must be a formula (specifying the body)")
  xs
}

get_args <- function(xs) {
  if (rlang::is_empty(xs))
    return(NULL)
  standardize_bare_arguments(xs)
}
standardize_bare_arguments <- function(xs) {
  no_name <- !nzchar(names(xs))
  names(xs)[no_name] <- vapply(xs[no_name], rlang::expr_name, character(1))
  xs[no_name] <- .BLANK
  xs
}

behead <- function(x) {
  list(head = get_head(x), body = rlang::f_rhs(x[[1]]))
}
get_head <- function(x) {
  nm <- names(x)
  arg <- rlang::f_lhs(x[[1]])
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
    rlang::abort("Default value of final argument is missing")
  NULL
}
get_nonempty_head <- function(arg, nm) {
  if (nzchar(nm))
    `names<-`(list(arg), nm)
  else
    `names<-`(.BLANK, rlang::expr_name(arg))
}

.BLANK <- list(quote(expr =))

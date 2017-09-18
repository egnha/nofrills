#' Low-cost anonymous functions
#'
#' `fn()` enables you to create (anonymous) functions, of arbitrary call
#' signature. Use it in place of the usual `function()` invocation whenever you
#' want to:
#' \itemize{
#'   \item type less:
#'     \preformatted{
#'     fn(x, y = 1 ~ x + y)
#'     function(x, y = 1) x + y}
#'     are equivalent
#'   \item guard against changes in lexical scope: by enabling tidyverse
#'     [quasiquotation][rlang::quasiquotation], `fn()` allows you to
#'     \dQuote{burn in} values at the point of function creation (see
#'     _Leveraging quasiquotation_)
#' }
#'
#' @param ... Function declaration, which supports
#'   [quasiquotation][rlang::quasiquotation].
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
#'   distinction is relevant for argument [splicing][rlang::UQS()], see below.)
#'
#'   - To the left of `~`, you write a conventional function-argument
#'     declaration, just as in `function(<arguments>)`: each of `arg1`, `arg2`,
#'     \dots, `argN` is either a bare argument (e.g., `x` or `...`) or an
#'     argument with default value (e.g., `x = 1`).
#'
#'   - To the right of `~`, you write the function body,
#'     i.e., an expression of the arguments.
#'
#'   \subsection{Quasiquotation}{
#'     All parts of a function declaration support tidyverse
#'     [quasiquotation][rlang::quasiquotation]:
#'     \itemize{
#'       \item To unquote values (of arguments or parts of the body), use `!!`
#'         or `UQ()`:
#'         \preformatted{
#'     z <- 0
#'     fn(x, y = !! z ~ x + y)
#'     fn(x ~ x > !! z)}
#'       \item To unquote argument names (with default value), use `:=`
#'         (definition operator):
#'         \preformatted{
#'     arg <- "y"
#'     fn(x, !! arg := 0 ~ x + !! as.name(arg))}
#'       \item To splice in a (formal) list of arguments, use `!!!` or `UQS()`:
#'         \preformatted{
#'     fn(!!! alist(x, y = 0), ~ x + y)}
#'         (Note that the body, in this case, must be given as a one-sided
#'         formula.)
#'       \item To write literal unquoting operators, use `QUQ()`, `QUQS()`,
#'         `QUQE()`:
#'         \preformatted{
#'     library(dplyr)
#'
#'     my_summarise <- fn(df, ... ~ {
#'       group_by <- quos(...)
#'       df \%>\%
#'         group_by(QUQS(group_by)) \%>\%
#'         summarise(a = mean(a))
#'     })}
#'         (Source:
#'         _[Programming with dplyr](http://dplyr.tidyverse.org/articles/programming.html)_)
#'     }
#'   }
#'
#' @section Leveraging quasiquotation: Functions in R are generally
#'   [impure](https://en.wikipedia.org/wiki/Pure_function), i.e., the return
#'   value of a function will _not_ in general be determined by the value of its
#'   inputs alone. This is because a function may depend on mutable objects in
#'   its
#'   [lexical scope](http://adv-r.hadley.nz/functions.html#lexical-scoping).
#'   Normally this isn’t an issue. But if you are working interactively and
#'   sourcing files into the global environment, say, it can be tricky to ensure
#'   that you haven’t unwittingly mutated an object that an earlier function
#'   depends upon.
#'
#'   **Example** — Consider the following function:
#'   ```
#'       a <- 1
#'       foo <- function(x) x + a
#'   ```
#'   What is the value of `foo(1)`? It is not necessarily `2`, because the value
#'   of `a` may have changed between the _creation_ of `foo()` and the _calling_
#'   of `foo(1)`:
#'   ```
#'       foo(1)  #> [1] 2
#'       a <- 0
#'       foo(1)  #> [1] 1
#'   ```
#'   In other words, `foo()` is impure because the value of `foo(x)` depends not
#'   only on the value of `x` but also on the _externally mutable_ value of `a`.
#'
#'   `fn()` enables you to write pure functions by using
#'   [quasiquotation](http://rlang.tidyverse.org/reference/quasiquotation.html)
#'   to eliminate such indeterminacy.
#'
#'   **Example** — With `fn()`, you can unquote `a` to \dQuote{burn in} its
#'   value at the point of creation:
#'   ```
#'       a <- 1
#'       foo <- fn(x ~ x + !! a)
#'   ```
#'   Now `foo()` is a pure function, unaffected by changes in its lexical scope:
#'   ```
#'       foo(1)  #> [1] 2
#'       a <- 0
#'       foo(1)  #> [1] 2
#'   ```
#'
#' @seealso [as_fn()], [make_fn_aware()]
#'
#' @examples
#' fn(x ~ x + 1)
#' fn(x, y ~ x + y)
#' fn(x, y = 2 ~ x + y)
#' fn(x, y = 1, ... ~ log(x + y, ...))
#'
#' ## to specify '...' in the middle, write '... = '
#' fn(x, ... = , y ~ log(x + y, ...))
#'
#' ## use one-sided formula for constant functions or commands
#' fn(~ NA)
#' fn(~ message("!"))
#'
#' ## unquoting is supported (using `!!` or UQ() from rlang)
#' zero <- 0
#' fn(x = UQ(zero) ~ x > !! zero)
#'
#' ## formals and function bodies can also be spliced in
#' f <- function(x, y) x + y
#' g <- function(y, x, ...) x - y
#' frankenstein <- fn(!!! formals(f), ~ !! body(g))
#' stopifnot(identical(frankenstein, function(x, y) x - y))
#'
#' ## mixing unquoting and literal unquoting is possible
#' if (suppressWarnings(require(dplyr))) {
#'   summariser <- quote(mean)
#'
#'   my_summarise <- fn(df, ... ~ {
#'     group_by <- quos(...)
#'     df %>%
#'       group_by(QUQS(group_by)) %>%
#'       summarise(a = UQ(summariser)(a))
#'   })
#'
#'   my_summarise
#' }
#'
#' @export
fn <- function(..., ..env = parent.frame()) {
  if (!is.environment(..env))
    abort("'..env' must be an environment")
  d <- get_fn_declaration(...)
  new_function(d$args, d$body, ..env)
}
#' @rdname fn
#' @export
#' @usage NULL
.. <- function(..., ..env = parent.frame()) {
  warn("`..()` is deprecated. Please use `fn()` instead.")
  fn(..., ..env = ..env)
}

get_fn_declaration <- function(...) {
  xs <- get_exprs(...)
  args <- get_args(xs$front)
  remains <- behead(xs$back)
  list(args = c(args, remains$head), body = remains$body)
}

get_exprs <- function(...) {
  xs <- validate(exprs_(...))
  n <- length(xs)
  list(front = xs[-n], back = xs[n])
}
validate <- function(xs, n) {
  if (is_empty(xs))
    abort("No function specified")
  n <- length(xs)
  is_fml <- vapply(xs, is_formula, logical(1))
  if (any(is_fml[-n]))
    abort("Only the body (as last argument) should be a formula")
  if (!is_fml[n])
    abort("Final argument must be a formula (specifying the body)")
  xs
}

get_args <- function(xs) {
  if (is_empty(xs))
    return(NULL)
  standardize_bare_arguments(xs)
}
standardize_bare_arguments <- function(xs) {
  no_name <- !nzchar(names(xs))
  names(xs)[no_name] <- vapply(xs[no_name], expr_name, character(1))
  xs[no_name] <- .BLANK
  xs
}

behead <- function(x) {
  list(head = get_head(x), body = f_rhs(x[[1]]))
}
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

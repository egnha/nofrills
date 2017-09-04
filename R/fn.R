#' Low-cost anonymous functions
#'
#' `fn()` enables you to create (anonymous) functions, of arbitrary call
#' signature. It is a drop-in replacement for the usual
#' `function(<arguments>) <body>` invocation, but costs less:
#' \itemize{
#'   \item It is **shorter**:
#'     \preformatted{
#'     fn(x, y = 1 ~ x + y)
#'     function(x, y = 1) x + y}
#'     are equivalent.
#'   \item It is **safer**: by enabling (Tidyverse)
#'     [quasiquotation][rlang::quasiquotation], `fn()` allows you to
#'     \dQuote{burn in} values, which can guard your function from unexpected
#'     scope changes (see _Examples_).
#' }
#' To reduce visual noise, `..()` is provided as an alias of `fn()`.
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
#'   distinction is relevant for argument [splicing][rlang::UQS()]; see below.)
#'
#'   To the left of `~`, you write a conventional function-argument declaration,
#'   just as in `function(<arguments>)`: each of `arg1`, `arg2`, \dots, `argN`
#'   is either a bare argument (e.g., `x` or `...`) or an argument with default
#'   value (e.g., `x = 1`). To the right of `~`, you write the function body,
#'   i.e., an expression of the arguments.
#'
#'   \subsection{Quasiquotation}{
#'     All parts of a function declaration support (Tidyverse)
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
#'     }
#'   }
#'
#' @section What’s the point of quasiquotation?:
#'   Functions in R generally violate a basic tenet of
#'   [functional programming](http://adv-r.hadley.nz/functional-programming.html):
#'   they are [impure](https://en.wikipedia.org/wiki/Pure_function). In simple
#'   terms, this means that the return value of a function will _not_ in general
#'   be determined by the value of its inputs alone. The reason is that a
#'   function’s behavior can be mutated by changes in its
#'   [lexical scope](http://adv-r.hadley.nz/functions.html#lexical-scoping).
#'   This makes it trickier to reason about your code and ensure that functions
#'   do what you intend.
#'
#'   **Example** — Consider the following function:
#'   ```
#'       a <- 1
#'       foo <- function(x) x + a
#'   ```
#'   What is the value of `foo(1)`? It is not necessarily `2`, because the value
#'   of `a` may have changed between the _creation_ of `foo()` and the _calling_
#'   of `foo(1)`.
#'   ```
#'       foo(1)  #> [1] 2
#'       a <- 0
#'       foo(1)  #> [1] 1
#'   ```
#'   In other words, the value of `foo(x)` does not depend solely on the value
#'   of `x`, because `foo()` has a \dQuote{hidden} dependence on the _mutable_
#'   object `a`.
#'
#'   `fn()` eliminates such indeterminacy by enabling
#'   [quasiquotation][rlang::quasiquotation].
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
#' @seealso [as_fn()]
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
#' ## unquoting protects against changes in a function’s scope
#' x <- "x"
#' f <- function() x
#' f_solid <- fn(~ !! x)
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
fn <- function(..., ..env = parent.frame()) {
  if (!is.environment(..env))
    abort("'..env' must be an environment")
  d <- get_fn_declaration(...)
  new_function(d$args, d$body, ..env)
}
#' @rdname fn
#' @export
.. <- fn

get_fn_declaration <- function(...) {
  xs <- get_exprs(...)
  args <- get_args(xs$front)
  remains <- behead(xs$back)
  list(args = c(args, remains$head), body = remains$body)
}

get_exprs <- function(...) {
  xs <- validate(exprs(...))
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

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
#'     _Pure functions via quasiquotation_)
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
#'   distinction is relevant for argument [splicing][rlang::quasiquotation], see
#'   below.)
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
#'       \item To unquote values (of arguments or parts of the body), use `!!`:
#'         \preformatted{
#'     z <- 0
#'     fn(x, y = !!z ~ x + y)
#'     fn(x ~ x > !!z)}
#'       \item To unquote argument names (with default value), use `:=`
#'         (definition operator):
#'         \preformatted{
#'     arg <- "y"
#'     fn(x, !!arg := 0 ~ x + !!as.name(arg))}
#'       \item To splice in a (formal) list of arguments, use `!!!`:
#'         \preformatted{
#'     fn(!!!alist(x, y = 0), ~ x + y)}
#'         (Note that the body, in this case, must be given as a one-sided
#'         formula.)
#'       \item To write literal unquoting operators, use `QUQ()`, `QUQS()`:
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
#' @section Pure functions via quasiquotation: Functions in R are generally
#'   [impure](https://en.wikipedia.org/wiki/Pure_function), i.e., the return
#'   value of a function will _not_ in general be determined by the value of its
#'   inputs alone. This is because a function may depend on mutable objects in
#'   its
#'   [lexical scope](http://adv-r.hadley.nz/functions.html#lexical-scoping).
#'   Normally this isn’t an issue. But if you are working interactively and
#'   sourcing files into the global environment, say, or using a notebook
#'   interface
#'   (like [Jupyter](https://jupyter.org) or
#'   [R Notebook](http://rmarkdown.rstudio.com/r_notebooks.html)),
#'   it can be tricky to ensure that you haven’t unwittingly mutated an object
#'   that an earlier function depends upon.
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
#'   `fn()` enables you to write _pure_ functions by using
#'   [quasiquotation](http://rlang.tidyverse.org/reference/quasiquotation.html)
#'   to eliminate such indeterminacy.
#'
#'   **Example** — With `fn()`, you can unquote `a` to \dQuote{burn in} its
#'   value at the point of creation:
#'   ```
#'       a <- 1
#'       foo <- fn(x ~ x + !!a)
#'   ```
#'   Now `foo()` is a pure function, unaffected by changes in its lexical scope:
#'   ```
#'       foo(1)  #> [1] 2
#'       a <- 0
#'       foo(1)  #> [1] 2
#'   ```
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
#' ## unquoting is supported (using `!!` from rlang)
#' zero <- 0
#' fn(x = !!zero ~ x > !!zero)
#'
#' ## formals and function bodies can also be spliced in
#' f <- function(x, y) x + y
#' g <- function(y, x, ...) x - y
#' frankenstein <- fn(!!!formals(f), ~ !!body(g))
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
#'       summarise(a = `!!`(summariser)(a))
#'   })
#'
#'   my_summarise
#' }
#'
#' @name fn
NULL

fn_constructor <- function(get_exprs) {
  force(get_exprs)

  function(..., ..env = parent.frame()) {
    is.environment(..env) %because% "'..env' must be an environment"
    fun <- fn_parts(get_exprs(...))
    make_function(fun$args, fun$body, ..env)
  }
}

fn_parts <- function(xs) {
  n <- length(xs)
  validate(xs, n)
  args <- get_args(xs[-n])
  remains <- behead(xs[n])
  list(args = c(args, remains$head), body = remains$body)
}

validate <- function(xs, n = length(xs)) {
  (n > 0L) %because%
    "Function must be declared"
  is_fml <- vapply(xs, is_formula, TRUE)
  all(!is_fml[-n]) %because%
    "Only the body (as last argument) should be a formula"
  is_fml[n] %because%
    "Final argument must be a formula (specifying the body)"
  invisible(xs)
}

get_args <- function(xs) {
  if (is_empty(xs))
    return(NULL)
  no_name <- !nzchar(names(xs))
  names(xs)[no_name] <- vapply(xs[no_name], expr_name, "")
  xs[no_name] <- blank
  xs
}

behead <- function(x) {
  nm <- names(x)
  fml <- x[[1L]]
  if (is_onesided(fml)) {
    (nm == "") %because% "Default value of final argument expected"
    head <- NULL
  } else {
    head <- nonempty_head(fml, nm)
  }
  list(head = head, body = f_rhs(fml))
}
nonempty_head <- function(fml, nm) {
  expr <- f_lhs(fml)
  if (nzchar(nm))
    return(list(expr) %named% nm)
  blank %named% expr_name(expr)
}

make_function <- function(args, body, env) {
  if (is_closure(body)) {
    body <- call("function", formals(body), base::body(body))
  } else {
    is_expr(body) %because% "Body must be an expression or closure"
  }
  new_fn(args, body, env)
}

blank <- list(quote(expr = ))

#' Raw quotation of expressions
#'
#' `literal_tidy()` is an extension of [rlang::exprs()] that comprehends literal
#' unquoting operators: `QUQ()`, `QUQS()` are substituted as `` `!!`() ``,
#' `` `!!!`() ``, resp.
#'
#' @param ... Unevaluated expressions to capture.
#' @return List of expressions.
#'
#' @noRd
literal_tidy <- local({
  quo_get_expr_ <- function(x) {
    do.call("substitute", list(quo_get_expr(x), quq))
  }
  quq <- list(
    QUQ  = as.name("!!"),
    QUQS = as.name("!!!")
  )

  function(...) {
    lapply(quos(...), quo_get_expr_)
  }
})

#' @rdname fn
#' @export
fn <- fn_constructor(literal_tidy)

literal <- function(...) {
  exprs <- as.list(substitute(...()))
  exprs %named% names_chr(exprs)
}

#' @examples
#' # Lightweight metaprogramming
#' enforce <- fn_(cond ~ fn(. ~ {stopifnot(!!substitute(cond)); .}))
#' nonans <- enforce(!is.nan(.))
#' log_strict <- fn(x ~ nonans(log(x)))
#' \dontrun{
#' log_strict(2)   # 0.6931472
#' log_strict(-1)  # Error: !is.nan(.) is not TRUE}
#'
#' @rdname fn
#' @export
fn_ <- fn_constructor(literal)

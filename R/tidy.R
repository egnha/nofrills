#' Make a function tidy
#'
#' A **tidy function** is a function whose arguments support unquoting (`!!`)
#' and (`!!!`) splicing, i.e., [quasiquotation][rlang::quasiquotation]. `tidy()`
#' enhances a function by making it tidy.
#'
#' @param f Function.
#' @return `tidy()` returns a tidy function.
#' @examples
#' f <- function(x, y, ..., z = "z") list(x, y, z, ...)
#' f_tidy <- tidy(f)
#'
#' stopifnot(
#'   # Make ordinary calls as usual
#'   identical(
#'     f_tidy("x", "y", "w"),
#'     f("x", "y", "w")
#'   ),
#'   # Splice arguments using ‘!!!’
#'   identical(
#'     f_tidy(y = "y", !!!list("x", a = "a", "b", c = "c")),
#'     f(y = "y", "x", a = "a", "b", c = "c")
#'   ),
#'   # tidy() is the identity for tidy functions
#'   identical(
#'     f_tidy, tidy(f_tidy)
#'   )
#' )
#'
#' x <- local({
#'   val <- "local value"
#'   rlang::quo(val)
#' })
#' stopifnot(
#'   # Unquote values using ‘!!’ to evaluate them immediately
#'   identical(
#'     f_tidy(!!x, "y"),
#'     f("local value", "y")
#'   )
#' )
#' @export
tidy <- local({
  body_tidy <- quote({
    enquo <- call("__quo__", `[[<-`(sys.call(), 1L, `__pretidy__`))
    `__eval_tidy__`(eval(enquo, `__quo__`, parent.frame()))
  })
  funs <- list(
    `__eval_tidy__` = eval_tidy,
    `__quo__`       = list(`__quo__` = quo)
  )

  function(f) {
    f <- match.fun(f)
    fmls <- fml_args(f)
    if (is_tidy(f, fmls))
      return(f)
    env <- envir(f) %encloses% c(funs, `__pretidy__` = f)
    f_tidy <- new_fn(fmls, body_tidy, env)
    class(f_tidy) <- "TidyFunction" %subclass% class(f)
    f_tidy
  }
})

is_tidy <- function(f, fmls = fml_args(f)) {
  if (is_empty(fmls))
    return(TRUE)
  inherits(f, "TidyFunction")
}

#' @rdname tidy
#' @return `untidy()` recovers the “untidy” function underlying a function made
#'   by `tidy()`.
#' @examples
#' stopifnot(
#'   # untidy() recovers the “untidy” function underlying a tidy function
#'   identical(f, untidy(tidy(f))),
#'   identical(f, untidy(f))
#' )
#' @export
untidy <- function(f) {
  f <- match.fun(f)
  untidy_(f) %||% f
}

untidy_ <- getter("__pretidy__")

#' @export
print.TidyFunction <- function(x, ...) {
  cat("<Tidy Function>\n\n")
  print(untidy_(x))
  cat("\nRecover the function shown with 'untidy()'.")
  invisible(x)
}

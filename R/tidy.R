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
tidy <- function(f) {
  is.function(f) %because% "Only functions can be tidied"
  if (is_tidy_(f))
    return(f)
  `__pretidy__` <- f
  `__quo__` <- quo
  f_tidy <- function() {
    call <- `[[<-`(sys.call(), 1, `__pretidy__`)
    call <- eval(as.call(c(`__quo__`, call)), parent.frame())
    eval_tidy(call)
  }
  formals(f_tidy) <- formals(closure(f))
  class(f_tidy) <- c("TidyFunction", class(f))
  f_tidy
}

#' @rdname tidy
#' @param x Object to test: Is it a tidy function?
#' @return `is_tidy()` returns `TRUE` for void functions and functions made by
#'   `tidy()`, and `FALSE` otherwise.
#' @examples
#' stopifnot(
#'   is_tidy(tidy(f)),
#'   !is_tidy(f),
#'   !is_tidy(untidy(tidy(f)))
#' )
#' @export
is_tidy <- function(x) {
  if (is.function(x))
    return(is_tidy_(x))
  FALSE
}

is_tidy_ <- function(f) {
  if (is_empty(formals(closure(f))))
    return(TRUE)
  env <- environment(f)
  if (is.null(env))
    return(FALSE)  # Non-vacuous primitive functions are never tidy
  exists("__pretidy__", envir = env, mode = "function")
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
  is.function(f) %because% "Only functions can be untidied"
  untidy_(f) %||% f
}

untidy_ <- getter("__pretidy__", environment)

#' @export
print.TidyFunction <- function(x, ...) {
  cat("<Tidy Function>\n\n")
  print.default(untidy_(x))
  cat("\n(Apply 'untidy()' to recover the function shown)")
  invisible(x)
}

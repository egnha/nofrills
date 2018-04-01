#' Curry a function
#'
#' @description
#' `curry()` [curries](https://en.wikipedia.org/wiki/Currying) functions in a
#' manner congenial to R’s calling convention.
#'
#' @param f Function.
#'
#' @return `curry()` returns a curried function.
#'
#' @seealso [fn_curry()], [partial()]
#'
#' @examples
#' f <- function(x, y, ..., z = 3) c(x, y, z, ...)
#' fc <- curry(f)
#' stopifnot(
#'   identical(
#'     formals(fc(1)),
#'     formals(function(y, ..., z = 3) {})
#'   ),
#'   identical(fc(1)(2),     c(1, 2, 3)),
#'   identical(fc(1, 2),     c(1, 2, 3)),
#'   identical(fc(2, x = 1), c(1, 2, 3)),
#'   identical(
#'     formals(fc(1, a = 1)),
#'     formals(function(y, ..., z = 3) {})
#'   ),
#'   identical(fc(1, a = 1)(2), c(1, 2, 3, a = 1)),
#'   identical(fc(1, 2, a = 1), c(1, 2, 3, a = 1))
#' )
#'
#' stopifnot(
#'   !is_curried(f),
#'   is_curried(fc),
#'   is_curried(function() NULL),
#'   is_curried(function(x) NULL),
#'   is_curried(function(...) NULL),
#'   is_curried(function(x = 1, y = 2) NULL)
#' )
#'
#' double <- curry(`*`)(2)
#' stopifnot(double(3) == 6)
#'
#' @export
curry <- local({
  `__callable__` <- function(f) {
    fmls <- formals(f)
    length(fmls) == 0 || all_have_values(fmls)
  }
  partialize <- function(f) {
    force(f)
    # '__f' is function-argument name of partial()
    function(call) `[[<-`(call, "__f", f)
  }

  function(f) {
    f_closure <- as_closure(f)
    if (is_curried_(f_closure))
      return(f)
    `__uncurry__` <- f  # Sentinel value for uncurrying
    `__partialize__` <- call_in_caller_env(partial, partialize(f))
    f_curried <- function() {
      p <- `__partialize__`()
      if (`__callable__`(p))
        return(p())
      nofrills::curry(p)
    }
    formals(f_curried) <- formals(f_closure)
    f_curried
  }
})

#' @param x Object to test.
#'
#' @return `is_curried(x)` is `TRUE` when `x` is a curried function, and
#'   `FALSE`, otherwise.
#'
#' @rdname curry
#' @export
is_curried <- function(x) {
  if (is.function(x))
    return(is_curried_(as_closure(x)))
  FALSE
}

is_curried_ <- function(f) {
  fmls <- formals(f)
  length(fmls) <= 1 ||
    all_have_values(fmls) ||
    c("__uncurry__", "__partialize__") %are% names(environment(f))
}

all_have_values <- function(fmls)
  all(fmls[names_nondots(fmls)] != quote(expr = ))

#' @rdname curry
#' @export
uncurry <- local({
  uncurry_ <- fun_getter("__uncurry__")
  function(f) {
    is.function(f) %because% "Only functions can be uncurried"
    uncurry_(f) %||% f
  }
})

#' @description
#' `fn_curry()` produces a curried function from an [fn()]-style function
#' declaration, which supports [quasiquotation][rlang::quasiquotation] of a
#' function’s body and (default) argument values.
#'
#' @examples
#' fn_curry(x, y, z = 0 ~ x + y + z)
#' fn_curry(target, x, ... ~ identical(x, target, ...))
#'
#' # Assign objects a class
#' classify_as <- fn_curry(class, x ~ `class<-`(x, class))
#' as_this <- classify_as("this")
#' stopifnot(inherits(as_this(NA), "this"))
#'
#' # Evaluate functions on a given set of arguments
#' do_call <- fn_curry(... = , ..f ~ do.call(..f, list(...)))
#' apply_fn <- do_call(1, 2)
#' stopifnot(
#'   apply_fn(..f = `*`) == 2,
#'   apply_fn(..f = `/`) == 0.5,
#'   apply_fn(..f = c)   == c(1, 2)
#' )
#'
#' @rdname fn
#' @export
fn_curry <- fn %>>>% curry

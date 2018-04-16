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
#'
#' stopifnot(
#'   identical(
#'     formals(fc(1)),
#'     formals(function(y, ..., z = 3) {})
#'   ),
#'   identical(
#'     formals(fc(1, a = 1)),
#'     formals(function(y, ..., z = 3) {})
#'   ),
#'   identical(
#'     formals(fc(y = 2, z = 4)),
#'     formals(function(x, ...) {})
#'   ),
#'   identical(fc(1)(2),        c(1, 2, 3)),
#'   identical(fc(y = 2)(1),    c(1, 2, 3)),
#'   identical(fc(1, 2),        c(1, 2, 3)),
#'   identical(fc(2, x = 1),    c(1, 2, 3)),
#'   identical(fc(1, 2, a = 1), c(1, 2, 3, a = 1)),
#'   identical(fc(1, a = 1)(2), c(1, 2, 3, a = 1)),
#'   identical(fc(1)(a = 1)(2), c(1, 2, 3, a = 1))
#' )
#'
#' stopifnot(
#'   !is_curried(f),
#'   !is_curried(uncurry(fc)),
#'   is_curried(fc),
#'   is_curried(fc(1)),
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
  `__curry__` <- function(f) {
    f_closure <- closure(f)
    if (is_curried_(f_closure))
      return(f)
    `__precurry__` <- f
    `__nms_unset_fmls__` <- names_unset_formals(f_closure)
    `__curry_partial__` <- curry_partial(f_closure, f, substitute(f))
    f_curried <- function() {
      mc <- match.call()
      if (length(mc) == 1)
        return(`__precurry__`())
      if (`__nms_unset_fmls__` %are% names(mc[-1]))
        return(eval(`[[<-`(mc, 1, `__precurry__`), parent.frame()))
      `__curry_partial__`()
    }
    formals(f_curried) <- formals(f_closure)
    class(f_curried) <- "CurriedFunction" %subclass% class(f)
    f_curried
  }

  names_unset_formals <- function(f) {
    fmls <- formals(f)
    nms <- names(fmls)
    nms[nms != "..." & fmls[] == quote(expr = )]
  }

  curry_partial <- function(f_closure, f, expr) {
    expr_curry <- expr_partial(f) %||% expr_fn(expr, f_closure)

    function() {
      call <- `[[<-`(sys.call(-1), "__f", f_closure)
      p <- eval(`[[<-`(call, 1, partial), parent.frame(2))
      expr_partial(p) <- expr_curry
      `__curry__`(p)
    }
  }

  `__curry__`
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
    return(is_curried_(closure(x)))
  FALSE
}

is_curried_ <- function(f) {
  fmls <- formals(f)
  length(fmls) <= 1 ||
    all(fmls[names(fmls) != "..."] != quote(expr = )) ||
    exists("__precurry__", envir = environment(f), mode = "function")
}

#' @rdname curry
#' @export
uncurry <- function(f) {
  is.function(f) %because% "Only functions can be uncurried"
  uncurry_(f) %||% f
}

uncurry_ <- getter("__precurry__", environment)

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
fn_curry <- function(..., ..env = parent.frame()) {
  curry(fn(..., ..env = ..env))
}

#' @export
print.CurriedFunction <- function(x, ...) {
  cat("<Curried Function>\n\n")
  uc <- uncurry_(x)
  if (inherits(uc, "PartialFunction"))
    expr_print(expr_partial_closure(uc))
  else
    print.default(closure(uc))
  cat("\n(Apply 'uncurry()' to restore conventional calling behavior)")
  invisible(x)
}

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
#' f <- function(x, y, z = 3) c(x, y, z)
#' fc <- curry(f)
#'
#' stopifnot(
#'   identical(
#'     formals(fc(1)),
#'     formals(function(y, z = 3) {})
#'   ),
#'   identical(
#'     formals(fc(y = 2, z = 4)),
#'     formals(function(x) {})
#'   ),
#'   identical(fc(1)(2),     c(1, 2, 3)),
#'   identical(fc(y = 2)(1), c(1, 2, 3)),
#'   identical(fc(1, 2),     c(1, 2, 3)),
#'   identical(fc(2, x = 1), c(1, 2, 3))
#' )
#'
#' stopifnot(
#'   !is_curried(f),
#'   !is_curried(uncurry(fc)),
#'   is_curried(fc),
#'   is_curried(fc(1)),
#'   is_curried(function() NULL),
#'   is_curried(function(x) NULL),
#'   !is_curried(function(...) NULL),
#'   !is_curried(function(x, y = 2) NULL),
#'   is_curried(function(x = 1, y = 2) NULL)
#' )
#'
#' double <- curry(`*`)(2)
#' stopifnot(double(3) == 6)
#'
#' @export
curry <- function(f) {
  f_closure <- closure(f)
  fmls <- formals(f_closure)
  if (is_curried_(f_closure, fmls))
    return(f)
  env <- environment(f_closure) %encloses% list(
    `__precurry__`      = f_closure,
    `__call_complete__` = check_call_complete(fmls),
    `__curry_partial__` = curry_partial(f_closure, f, substitute(f), fmls)
  )
  f_curried <- new_fn(fmls, body_curry(fmls), env)
  class(f_curried) <- "CurriedFunction" %subclass% class(f)
  f_curried
}

body_curry <- local({
  dots <- quote({
    if (length(mc <- match.call()) == 1L)
      return(`__precurry__`())
    `__curry_partial__`(mc, parent.frame())
  })
  nondots <- quote({
    if (length(mc <- match.call()) == 1L)
      return(`__precurry__`())
    if (`__call_complete__`(mc))
      return(eval(`[[<-`(mc, 1L, `__precurry__`), parent.frame()))
    `__curry_partial__`(mc, parent.frame())
  })

  function(fmls) {
    if (has_dots(names(fmls))) dots else nondots
  }
})

check_call_complete <- function(fmls) {
  nms <- names(fmls)
  if (has_dots(nms))
    return(NULL)
  nms_unset <- nms[fmls[] == quote(expr = )]
  function(mc) {
    nms_unset %are% names(mc)
  }
}

curry_partial <- function(f_closure, f, expr, fmls) {
  force(f_closure)
  expr_curry <- expr_partial(f) %||% expr_fn(expr, fmls)

  function(mc, env) {
    call <- `[[<-`(mc, "__f", f_closure)
    p <- eval(`[[<-`(call, 1L, partial), env)
    expr_partial(p) <- expr_curry
    curry(p)
  }
}

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

is_curried_ <- function(f, fmls = formals(f)) {
  inherits(f, "CurriedFunction") || (!has_dots(names(fmls)) && all_set(fmls))
}
all_set <- function(fmls) {
  length(fmls) <= 1 || all(fmls[] != quote(expr = ))
}

#' @rdname curry
#' @export
uncurry <- function(f) {
  is.function(f) %because% "Only functions can be uncurried"
  uncurry_(f) %||% f
}

uncurry_ <- getter_env("__precurry__")

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
#' do_call <- fn_curry(args, ..f ~ do.call(..f, args))
#' apply_fn <- do_call(list(1, 2))
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
  cat("<Curried Function>\n")
  cat("(Restore conventional calling behavior with `uncurry()`)\n\n")
  uc <- uncurry_(x)
  if (inherits(uc, "PartialFunction")) {
    expr_print(expr_partial_closure(uc))
  } else {
    print(closure(uc))
  }
  invisible(x)
}

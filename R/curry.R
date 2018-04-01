#' Curry a function
#'
#' @description
#' `curry()` [curries](https://en.wikipedia.org/wiki/Currying) functions—it
#' reconstitutes a function as a succession of single-argument functions. For
#' example, `curry()` produces the function
#' ```
#' function(x) {
#'   function(y) {
#'     function(z) {
#'       x * y * z
#'     }
#'   }
#' }
#' ```
#' from the function `function(x, y, z) x * y * z`.
#'
#' `fn_curry()` produces a curried function from an [fn()]-style function
#' declaration, which supports [quasiquotation][rlang::quasiquotation] of a
#' function’s body and (default) argument values.
#'
#' @details Dots (`...`) are treated as a unit when currying. For example,
#'   `curry()` transforms `function(x, ...) list(x, ...)` to
#'   `function(x) { function(...) list(x, ...) }`.
#'
#' @param f Function.
#'
#' @return Curried function.
#'
#' @seealso [fn_curry()], [partial()]
#'
#' @examples
#' curry(function(x, y, z = 0) x + y + z)
#' double <- curry(`*`)(2)
#' double(3)  # 6
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

all_have_values <- function(fmls)
  all(fmls[names_nondots(fmls)] != quote(expr = ))

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
    c("__uncurry__", "__partialize__") %in% names(environment(f))
}

#' @rdname curry
#' @export
uncurry <- local({
  uncurry_ <- fun_getter("__uncurry__")
  function(f) {
    is.function(f) %because% "Only functions can be uncurried"
    uncurry_(f) %||% f
  }
})

#' @examples
#' fn_curry(x, y, z = 0 ~ x + y + z)
#' fn_curry(target, x, ... ~ identical(x, target, ...))
#'
#' # Assign objects a class
#' classify_as <- fn_curry(class, x ~ `class<-`(x, class))
#' as_this <- classify_as("this")
#' as_this(NA)
#'
#' # Evaluate functions on a given set of arguments
#' do_call <- fn_curry(... = , ..f ~ do.call(..f, list(...)))
#' apply_fn <- do_call(1, 2)
#' apply_fn(..f = `*`)
#' apply_fn(..f = `/`)
#' apply_fn(..f = c)
#'
#' @rdname fn
#' @export
fn_curry <- fn %>>>% curry

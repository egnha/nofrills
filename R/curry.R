#' Curry a function
#'
#' @return The original function as nested calls of functions of successive
#'   arguments.
#'
#' @examples
#' curry(`*`)
#' curry(function(x, y, z = 0) x + y + z)
#' curry_fn(x, y, z = 0 ~ x + y + z)
#' curry_fn(target, ... ~ identical(target, ...))
#' compare_to <- curry_fn(target, x ~ identical(target, x))
#' is_this <- compare_to("this")
#' is_this("that")
#' is_this("this")
#'
#' @name curry
NULL

#' @param ... Function declaration, which supports
#'   [quasiquotation][rlang::quasiquotation].
#' @param ..env Environment in which to create the function (i.e., the
#'   function’s [enclosing environment][base::environment]).
#'
#' @rdname curry
#' @export
curry_fn <- function(..., ..env = parent.frame()) {
  curry(fn(..., ..env = ..env))
}

#' @param f Function.
#' @param env Environment of the curried function or `NULL`. If `NULL`, the
#'   environment of the curried function is the calling environment.
#'
#' @rdname curry
#' @export
curry <- local({
  each <- function(xs)
    lapply(seq_along(xs), function(i) xs[i])
  lambda <- function(x, body)
    call("function", as.pairlist(x), body)

  function(f, env = environment(f)) {
    stopifnot(is.function(f), is.environment(env) || is.null(env))
    f <- as_closure(f)
    fmls <- formals(f)
    if (length(fmls) < 2)
      return(f)
    curry_expr <- Reduce(lambda, each(fmls), body(f), right = TRUE)
    env <- env %||% parent.frame()
    eval(curry_expr, env)
  }
})

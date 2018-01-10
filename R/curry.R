#' Curry a function
#'
#' @param f Function.
#' @param env Environment of the curried function or `NULL`. If `NULL`, the
#'   environment of the curried function is the calling environment.
#'
#' @return The original function as nested calls of functions of successive
#'   arguments.
#'
#' @seealso [fn()]
#'
#' @examples
#' curry(`*`)
#' curry(function(x, y, z = 0) x + y + z)
#' curry_fn(x, y, z = 0 ~ x + y + z)
#' curry_fn(target, ... ~ identical(target, ...))
#' compare_to <- curry_fn(target, x ~ identical(QUQ(target), x))
#' is_this <- compare_to("this")
#' is_this("that")  # FALSE
#' is_this("this")  # TRUE
#'
#' @export
curry <- function(f, env = environment(f)) {
  stopifnot(is.function(f), is.environment(env) || is.null(env))
  f <- as_closure(f)
  fmls <- formals(f)
  if (length(fmls) < 2)
    return(f)
  curry_(fmls, body(f), env %||% parent.frame())
}

curry_ <- local({
  each <- function(xs)
    lapply(seq_along(xs), function(i) xs[i])
  lambda <- function(x, body)
    call("function", as.pairlist(x), body)

  function(args, body, env) {
    curry_expr <- Reduce(lambda, each(args), body, right = TRUE)
    eval(curry_expr, env)
  }
})

make_curried_function <- local({
  fn_call <- function(arg, body)
    as.call(c(quote(fn), as.pairlist(arg), bquote(~.(body))))

  function(args, body, env) {
    n <- length(args)
    if (n < 2)
      return(make_function(args, body, env))
    terminal_body <- fn_call(args[n], body)
    curry_(args[-n], terminal_body, env)
  }
})

#' @param ... Function declaration, which supports
#'   [quasiquotation][rlang::quasiquotation].
#' @param ..env Environment in which to create the function (i.e., the
#'   function’s [enclosing environment][base::environment]).
#'
#' @rdname curry
#' @export
curry_fn <- fn_factory(make_curried_function)

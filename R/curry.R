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
#' `curry_fn()` produces a curried function from an [fn()]-style function
#' declaration, which supports [quasiquotation][rlang::quasiquotation] of a
#' function’s body and (default) argument values.
#'
#' @details Dots (`...`) are treated as a unit when currying. For example,
#'   `curry()` transforms `function(x, ...) list(x, ...)` to
#'   `function(x) { function(...) list(x, ...) }`.
#'
#' @param f Function.
#' @param env Environment of the curried function or `NULL`. If `NULL`, the
#'   environment of the curried function is the calling environment.
#'
#' @return A function of nested single-argument functions.
#'
#' @seealso [fn()], [partial()]
#'
#' @examples
#' curry(function(x, y, z = 0) x + y + z)
#' double <- curry(`*`)(2)
#' double(3)  # 6
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
    as.call(c(quote(nofrills::fn), as.pairlist(arg), bquote(~.(body))))

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
#' @examples
#' curry_fn(x, y, z = 0 ~ x + y + z)
#' curry_fn(target, ... ~ identical(target, ...))
#'
#' ## Delay unquoting to embed argument values into the innermost function.
#' ## This makes generated source code readily comprehensible.
#'
#' # Assign objects a class
#' classify_as <- curry_fn(class, x ~ `class<-`(x, QUQ(class)))
#' as_this <- classify_as("this")
#' as_this
#' #> function (x)
#' #> `class<-`(x, "this")
#'
#' # Evaluate functions on a given set of arguments
#' do_call <- curry_fn(... = , f ~ f(QUQS(list(...))))
#' do_call(x = 1, 2)
#' #> function (f)
#' #> f(x = 1, 2)
#'
#' @rdname curry
#' @export
curry_fn <- fn_factory(make_curried_function)

#' @rdname curry
#' @export
curry2 <- local({
  `__callable__` <- function(f) {
    fmls <- formals(f)
    length(fmls) == 0 || all(fmls[names_nondots(fmls)] != quote(expr = ))
  }

  function(f) {
    fmls <- formals(as_closure(f))
    if (length(fmls) <= 1)
      return(f)
    `__uncurry__` <- f
    `__partialize__` <- function() {
      call_partial <- `[[<-`(sys.call(-1), 1, partial)
      call_partial$..f <- f
      eval(call_partial, parent.frame(2))
    }
    f_curried <- function() {
      p <- `__partialize__`()
      if (`__callable__`(p))
        return(p())
      nofrills::curry2(p)
    }
    formals(f_curried) <- fmls
    f_curried
  }
})

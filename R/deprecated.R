#' Deprecated functions
#'
#' These functions are softly deprecated and may be removed from a future
#' version of \pkg{nofrills}. They were intended to enable a shorter
#' `fn()`-syntax for higher-order functions—a negligible convenience. Instead,
#' simply invoke `fn()` directly.
#'
#' @param f `make_fn_aware()`: Function, or symbol or name of a function;
#'   `curry()`: Function.
#' @param ... `make_fn_aware()`: Name(s) of functional argument(s) of `f`
#'   (strings) or `NULL`. Unsplicing of lists of strings is supported via `!!!`;
#'   `curry_fn()`: Function declaration, which supports
#'   [quasiquotation][rlang::quasiquotation].
#'
#' @seealso [fn()], [partial()]
#'
#' @keywords internal
#' @aliases as_fn make_fn_aware
#' @name deprecated
NULL

#' @details `as_fn()` is for functions that take functional arguments. Use
#'   `as_fn()` _inside_ a function to enable it to comprehend a minimal
#'   anonymous-function notation for arguments that are functions. This notation
#'   is that of [fn()], but with \sQuote{`fn`} replaced by \sQuote{`.`} (dot).
#'   `as_fn()` cannot follow promise expressions across function calls. It is
#'   only intended to work in the immediate context in which a function
#'   declaration is to be interpreted (see _Examples_).
#'
#' @param .f A function or an abbreviated anonymous-function expression of the
#'   form `.(...)`, where `...` is a [function declaration][fn()] (i.e., `.`
#'   (dot) in this context is an alias of [fn()]).
#'   [Quasiquotation][rlang::quasiquotation] is supported.
#'
#' @return `as_fn()`: If `.f` is a function, it is simply returned, otherwise
#'   the function determined by the [function declaration][fn()] is returned.
#'
#' @examples
#' \dontrun{
#' call_fn <- function(.f, x) {
#'   f <- as_fn(.f)
#'   f(x)
#' }
#' call_fn(log, 1)
#' call_fn(.(. ~ sin(.) ^ 2), 1)
#' # simplified function expressions support quasiquotation
#' f <- sin
#' call_fn(.(. ~ (!!f)(.) ^ 2), 1)
#'
#' ## wrap Map() to accept abbreviated anonymous function expressions
#' Map_ <- function (f, ...) {
#'   f <- as_fn(f)
#'   mapply(FUN = f, ..., SIMPLIFY = FALSE)
#' }
#' # you can call Map_() just like Map()
#' Map_(function(x, y, z) paste(x, y, paste("and", z), sep = ", "), 1:3, 4:6, 7:9)
#' # or use a simplified function expression
#' Map_(.(x, y, z ~ paste(x, y, paste("and", z), sep = ", ")), 1:3, 4:6, 7:9)
#'
#' ## abbreviated anonymous functions are interpreted in the calling environment
#' # so this works, as expected
#' foo <- function(a) as_fn(a)
#' foo(.(x ~ x + 1))
#' # but as_fn() can't interpret abbreviated anonymous functions across calls
#' foo <- function(a) bar(a)
#' bar <- function(b) as_fn(b)
#' foo(.(x ~ x + 1))
#' }
#'
#' @rdname deprecated
#' @export
as_fn <- function(.f) {
  .Deprecated("fn")
  x <- enexpr(.f)
  x <- eval(substitute(substitute(x)), parent.frame())
  interpret_fn(x, match.fun(.f), parent.frame(2L))
}

interpret_fn <- function(x, f = x, env) {
  if (is.call(x) && is_anon_fn_expr(x)) {
    x[[1L]] <- fn
    eval(x, env)
  } else
    f
}
is_anon_fn_expr <- check_head(".")

#' @details `make_fn_aware()` is a functional operator that enhances a function
#'   by enabling it to interpret abbreviated functional arguments.
#'
#' @return `make_fn_aware()`: A function with the same call signature as `f`,
#'   but whose function arguments, as designated by `...`, may be specified
#'   using an abbreviated function expression of the form `.(...)`, cf.
#'   [as_fn()]. If `...` is empty or `NULL`, then `f` is simply returned.
#'
#' @examples
#' \dontrun{
#' reduce <- make_fn_aware(Reduce, "f")
#'
#' ## reduce() behaves just like Reduce()
#' Reduce(function(u, v) u + 1 / v, c(3, 7, 15, 1, 292), right = TRUE)
#' reduce(function(u, v) u + 1 / v, c(3, 7, 15, 1, 292), right = TRUE)
#'
#' ## reduce() can also interpret abbreviated function expressions
#' reduce(.(u, v ~ u + 1 / v), c(3, 7, 15, 1, 292), right = TRUE)
#' }
#'
#' @rdname deprecated
#' @export
make_fn_aware <- function(f, ...) {
  .Deprecated("fn")
  f <- match.fun(f)
  fmls <- fn_fmls(f)
  interpret_anon_fns <- anon_fn_interpreter(names(fmls), ...)
  if (is.null(interpret_anon_fns))
    return(f)
  `formals<-`(
    function() {
      env_encl <- parent.env(environment())
      env_call <- parent.frame()
      call <- interpret_anon_fns(match.call(), env_call)
      call[[1L]] <- env_encl$f
      eval(call, env_call)
    },
    value = fmls
  )
}

anon_fn_interpreter <- function(nms, ...) {
  nms_fn <- chr(...)
  # detect emptiness here, rather than in caller, so that empty splice is empty
  if (is_empty(nms_fn))
    return(NULL)
  nms_fn %are% nms %because% "Name(s) must be those of argument(s)"
  function(call, env) {
    call[nms_fn] <- lapply(call[nms_fn], interpret_fn, env = env)
    call
  }
}

#' @details `curry()` [curries](https://en.wikipedia.org/wiki/Currying)
#'   functions—it reconstitutes a function as a succession of single-argument
#'   functions. For example, `curry()` produces the the function
#'   ```
#'   function(x) {
#'     function(y) {
#'       function(z) {
#'         x * y * z
#'       }
#'     }
#'   }
#'   ```
#'   from the function `function(x, y, z) x * y * z`. Dots (`...`) are treated
#'   as a unit when currying. For example, `curry()` transforms `function(x,
#'   ...) list(x, ...)` to `function(x) { function(...) list(x, ...) }`.
#'
#' @param env Environment of the curried function or `NULL`. If `NULL`, the
#'   environment of the curried function is the calling environment.
#'
#' @return `curry()`, `curry_fn()`: A function of nested single-argument
#'   functions.
#'
#' @examples
#' \dontrun{
#' curry(function(x, y, z = 0) x + y + z)
#' double <- curry(`*`)(2)
#' double(3)  # 6
#' }
#'
#' @rdname deprecated
#' @export
curry <- function(f, env = environment(f)) {
  .Deprecated(
    "partial",
    msg = "'curry' is deprecated. Instead, apply 'partial' repeatedly."
  )
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

#' @details `curry_fn()` produces a curried function from an [fn()]-style
#'   function declaration, which supports
#'   [quasiquotation][rlang::quasiquotation] of a function’s body and (default)
#'   argument values.
#'
#' @param ..env Environment in which to create the function (i.e., the
#'   function’s [enclosing environment][base::environment]).
#'
#' @examples
#' \dontrun{
#' curry_fn(x, y, z = 0 ~ x + y + z)
#' curry_fn(target, ... ~ identical(target, ...))
#'
#' ## Delay unquoting to embed argument values into the innermost function
#' compare_to <- curry_fn(target, x ~ identical(x, QUQ(target)))
#' is_this <- compare_to("this")
#' is_this("that")  # FALSE
#' is_this("this")  # TRUE
#' classify_as <- curry_fn(class, x ~ `class<-`(x, QUQ(class)))
#' as_this <- classify_as("this")
#' as_this("Some object")  # String of class "this"
#' }
#'
#' @rdname deprecated
#' @export
curry_fn <- function(..., ..env = parent.frame()) {
  .Deprecated(
    "partial",
    msg = "'curry_fn' is deprecated. Instead, apply 'partial' repeatedly."
  )
  is.environment(..env) %because% "'..env' must be an environment"
  fun <- fn_parts(literal_tidy(...))
  make_curried_function(fun$args, fun$body, ..env)
}

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

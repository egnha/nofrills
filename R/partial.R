#' Fix a number of arguments to a function
#'
#' @description
#' `partial()` enables
#' [partial function application](https://en.wikipedia.org/wiki/Partial_application):
#' given a function, it fixes the value of selected arguments to produce a
#' function of the remaining arguments.
#'
#' `departial()` \dQuote{inverts} the application of `partial()` by returning
#' the original function.
#'
#' @param ..f Function.
#' @param ... Argument values of `..f` to fix, specified by name. Captured as
#'   [quosures][rlang::quotation]. [Quasiquotation][rlang::quasiquotation] and
#'   [splicing][rlang::quasiquotation] are supported (see _Examples_).
#'
#' @return `partial()` returns a function whose [formals][base::formals()] are a
#'   literal truncation of the formals of `..f()` (as a closure) by the fixed
#'   arguments. `partial(..f)` is identical to `..f`.
#'
#' @section Technical Note:
#'   Even while `partial()` truncates formals, it remains compatible with
#'   functions that use \code{\link[base:missing]{missing()}} to test whether a
#'   specified argument was supplied in a call. For example,
#'   `draw3 <- partial(sample, size = 3)` works as a function that randomly
#'   draws three elements, even though `sample()` invokes `missing(size)` and
#'   `draw3()` has signature `function (x, replace = FALSE, prob = NULL)`.
#'
#'   Consequently, in rare cases, impure functions that depend on introspection
#'   of the calling context may not be amenable to `partial()`. For example,
#'   `partial(ls, all.names = TRUE)()` is not equivalent to
#'   `ls(all.names = TRUE)`, because `ls()` inspects the calling environment to
#'   produce its value and `partial(ls, all.names = TRUE)()` calls
#'   `ls(all.names = TRUE)` from an (ephemeral) execution environment.
#'
#' @seealso [curry()]
#'
#' @examples
#' draw3 <- partial(sample, size = 3)
#' draw3(letters)
#'
#' # Use departial() to recover the original function
#' stopifnot(identical(departial(draw3), sample))
#'
#' # Lazily evaluate argument values by default
#' # The value of 'n' is evaluated whenever rnd() is called.
#' rnd <- partial(runif, n = rpois(1, 5))
#' replicate(4, rnd(), simplify = FALSE)   # variable length
#'
#' # Eagerly evaluate argument values with unquoting (`!!`)
#' # The value of 'n' is fixed when 'rnd_eager' is created.
#' rnd_eager <- partial(runif, n = !! rpois(1, 5))
#' len <- length(rnd_eager())
#' reps <- replicate(4, rnd_eager(), simplify = FALSE)   # constant length
#' stopifnot(all(lengths(reps) == len))
#'
#' # Mix evaluation schemes by combining lazy evaluation with unquoting (`!!`)
#' # Here 'n' is lazily evaluated, while 'max' is eagerly evaluated.
#' rnd_mixed <- partial(runif, n = rpois(1, 5), max = !! sample(10, 1))
#' replicate(4, rnd_mixed(), simplify = FALSE)
#'
#' # Arguments to fix can be spliced
#' args_eager <- list(n = rpois(1, 5), max = sample(10, 1))
#' rnd_eager2 <- partial(runif, !!! args_eager)
#' replicate(4, rnd_eager2(), simplify = FALSE)
#'
#' args_mixed <- rlang::exprs(n = rpois(1, 5), max = !! sample(10, 1))
#' rnd_mixed2 <- partial(runif, !!! args_mixed)
#' replicate(4, rnd_mixed2(), simplify = FALSE)
#'
#' # partial() truncates formals by the fixed arguments
#' foo <- function(x, y = x, ..., z = "z") NULL
#' args(foo)
#' args(partial(foo))
#' args(partial(foo, x = 1))
#' args(partial(foo, x = 1, y = 2))
#' args(partial(foo, x = 1, y = 2, z = 3))
#'
#' @export
partial <- function(..f, ...) {
  fix <- quos(...)
  if (is_empty(fix))
    return(..f)
  . <- deconstruct(..f)
  names(fix) %are% names(.$fmls) %because%
    "Values to fix must be named by arguments of {..f}"
  partial_(.$fun, .$fmls, fix, .$env)
}

deconstruct <- function(..f) {
  f <- as_closure(..f)
  f_dp <- departial_(..f)
  if (is.null(f_dp)) {
    fun <- f
    env <- new.env(parent = environment(f))
  }
  else {
    fun <- f_dp
    env <- environment(f)
  }
  list(fun = fun, fmls = formals(f), env = env)
}

partial_ <- function(fun, fmls, fix, env) {
  bind_quosures_actively(fix, env)
  env$`__fun__` <- fun
  fmls_trunc <- truncate(fmls, names(fix))
  args <- eponymous(formals(fun))
  fn(!!! fmls_trunc, ~ `__fun__`(!!! args), ..env = env)
}

bind_quosures_actively <- function(qs, env) {
  for (nm in names(qs))
    makeActiveBinding(nm, get_tidy(qs[[nm]]), env)
}

get_tidy <- function(q) {
  force(q)
  function(.) eval_tidy(q)
}

truncate <- function(xs, nms)
  xs[!(names(xs) %in% nms)]

eponymous <- function(xs) {
  nms <- names(xs)
  `names<-`(lapply(nms, as.name), nms)
}

#' @rdname partial
#' @export
departial <- function(..f) {
  is.function(..f) %because% "Only functions can be de-partialized"
  departial_(..f) %||% ..f
}

departial_ <- function(..f)
  environment(..f)$`__fun__`

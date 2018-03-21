#' Fix a number of arguments to a function
#'
#' @description
#' `partial()` enables
#' [partial function application](https://en.wikipedia.org/wiki/Partial_application):
#' given a function, it fixes the value of selected arguments to produce a
#' function of the remaining arguments.
#'
#' `departial()` inverts the application of `partial()`.
#'
#' @param ..f Function.
#' @param ... Argument values of `..f` to fix, specified by name.
#'   [Quasiquotation][rlang::quasiquotation] and
#'   [splicing][rlang::quasiquotation] are supported (see _Examples_).
#' @param ..lazy Should the argument values be lazily evaluated? If `TRUE` (the
#'   default), the argument values are captured as expressions; if `FALSE`, the
#'   argument values are captured as [quosures][rlang::quosure] and are [tidily
#'   evaluated][rlang::eval_tidy].
#'
#' @return `partial()` returns a function whose [formals][base::formals()] are a
#'   contraction of the formals of `..f()` (as a closure) by the fixed
#'   arguments. `partial(..f)` is identical to `..f`.
#'
#' @section Technical Note:
#'   Even while `partial()` contracts formals, it remains compatible with
#'   functions that use \code{\link[base:missing]{missing()}} to test whether a
#'   specified argument was given in a call.
#'
#'   However, in rare cases, impure functions that depend on introspection of
#'   the calling environment may not be amenable to `partial()`. For example,
#'   `partial(ls, all.names = TRUE)()` is not equivalent to
#'   `ls(all.names = TRUE)`, because `ls()` inspects the calling environment to
#'   produce its value and `partial(ls, all.names = TRUE)()` calls
#'   `ls(all.names = TRUE)` from an (ephemeral) execution environment, where
#'   only the arguments of `partial(ls, all.names = TRUE)` are bound (as
#'   promises).
#'
#' @seealso [curry()]
#'
#' @examples
#' draw3 <- partial(sample, size = 3)
#' draw3(letters)
#'
#' # Use departial() to recover the original function
#' departial(draw3)  # sample()
#'
#' # Arguments are lazily evaluated by default, i.e., whenever rnd_lazy() is called
#' rnd_lazy <- partial(runif, n = rpois(1, 5))
#' replicate(4, rnd_lazy(), simplify = FALSE)   # variable length
#'
#' # Arguments can be eagerly evaluated, i.e., when rnd_eager() is created
#' rnd_eager <- partial(runif, n = rpois(1, 5), ..lazy = FALSE)
#' replicate(4, rnd_eager(), simplify = FALSE)  # constant length
#'
#' # Arguments can be eagerly evaluated, selectively, via unquoting
#' rnd <- partial(runif, n = !! rpois(1, 5), max = sample(10, 1))
#' replicate(4, rnd(), simplify = FALSE)
#'
#' # Arguments to fix can be spliced
#' args_eager <- list(n = rpois(1, 5), max = sample(10, 1))
#' rnd_eager3 <- partial(runif, !!! args_eager)
#' replicate(4, rnd_eager3(), simplify = FALSE)
#' args_mixed <- rlang::exprs(n = !! rpois(1, 5), max = sample(10, 1))
#' rnd2 <- partial(runif, !!! args_mixed)
#' replicate(4, rnd2(), simplify = FALSE)
#'
#' # partial() contracts formals (i.e., argument signature)
#' foo <- function(x, y = x, ..., z = "z") list(x = x, y = y, z = z, ...)
#' args(foo)
#' args(partial(foo))
#' args(partial(foo, x = 1))
#' args(partial(foo, x = 1, y = 2))
#' args(partial(foo, x = 1, y = 2, z = 3))
#'
#' @export
partial <- function(..f, ..., ..lazy = TRUE) {
  vals <- if (..lazy) exprs(...) else lapply(quos(...), eval_tidy)
  if (is_empty(vals))
    return(..f)
  f <- as_closure(..f)
  fmls <- formals(f)
  names(vals) %are% names(fmls) %because%
    "Values to fix must be named by arguments of {..f}"
  env <- new.env(parent = parent.frame())
  env$`__function__` <- f
  fmls <- contract(fmls, vals)
  vals <- c(vals, eponymous(names(fmls)))
  fn(!!! fmls, ~ `__function__`(!!! vals), ..env = env)
}

contract <- function(fmls, vals) {
  fmls <- fmls[!(names(fmls) %in% names(vals))]
  fmls <- lapply(fmls, subst, vals = vals)
  as.pairlist(fmls)
}
subst <- function(expr, vals)
  do.call("substitute", list(expr, vals))

eponymous <- function(nms)
  `names<-`(lapply(nms, as.name), nms)

#' @rdname partial
#' @export
departial <- function(..f) {
  is.function(..f) %because% "Only functions can be de-partialized"
  environment(..f)$`__function__` %||% ..f
}

#' Fix a number of arguments to a function
#'
#' `partial()` enables
#' [partial function application](https://en.wikipedia.org/wiki/Partial_application):
#' given a function, it fixes the value of selected arguments to produce a
#' function of the remaining arguments. `departial()` inverts the application of
#' `partial()`.
#'
#' @param ..f Function.
#' @param ... Argument values of `..f` to fix, specified by name.
#'   [Quasiquotation][rlang::quasiquotation] and splicing are supported; see
#'   _Examples_.
#' @param ..lazy Should the argument values be lazily evaluated? If `TRUE` (the
#'   default), the argument values are captured as expressions; if `FALSE`, the
#'   argument values are [tidily evaluated][rlang::eval_tidy].
#' @param ..env Environment in which to create the partialized function (i.e.,
#'   the function’s [enclosing environment][base::environment]).
#'
#' @return `partial()` returns a function of argument signature `function(...)`,
#'   which calls `..f()` on the fixed argument values followed by the dots. When
#'   no arguments are specified, `partial()` returns `..f`.
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
#' @export
partial <- function(..f, ..., ..lazy = TRUE, ..env = parent.frame()) {
  args <- if (..lazy) exprs(...) else lapply(quos(...), eval_tidy)
  if (is_empty(args))
    return(..f)
  f <- as_closure(..f)
  names(args) %are% names(formals(f)) %because%
    "Values to fix must be named by arguments of {..f}"
  env <- new.env(parent = ..env)
  env$.PartializedFunction <- f
  fn(... ~ .PartializedFunction(!!! args, ...), ..env = env)
}

#' @rdname partial
#' @export
departial <- function(..f) {
  is.function(..f) %because% "Only functions can be de-partialized"
  environment(..f)$.PartializedFunction %||% ..f
}

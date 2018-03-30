#' Fix a number of arguments to a function
#'
#' @description
#' `partial()` enables
#' [partial application](https://en.wikipedia.org/wiki/Partial_application):
#' given a function, it fixes the value of selected arguments to produce a
#' function of the remaining arguments.
#'
#' `departial()` \dQuote{inverts} the application of `partial()` by returning
#' the original function.
#'
#' @param __f Function.
#' @param ... Argument values of `` `__f` `` to fix, specified by name. Captured
#'   as [quosures][rlang::quotation]. [Quasiquotation][rlang::quasiquotation]
#'   and [splicing][rlang::quasiquotation] are supported (see _Examples_).
#'
#' @return `partial()` returns a function whose [formals][base::formals()] are a
#'   literal truncation of the formals of `` `__f`()`` (as a closure) by the
#'   fixed arguments. ``partial(`__f`)`` is identical to `` `__f` ``.
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
#' @seealso [curry()], [fn_curry()]
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
partial <- local({
  quos_match <- function(fmls) {
    mc <- match.call(fn_template_partial, sys.call(-1))
    dots <- mc[names(mc) != "__f"]
    eval(call_quos_match(dots, fmls), parent.frame(2))
  }

  function(`__f`, ...) {
    f <- as_closure(`__f`)
    fmls <- formals(f)
    fix <- quos_match(fmls)  # '...' consumed by introspection
    if (is_empty(fix))
      return(`__f`)
    parent <- environment(f)
    names(fix) <- name_bare_dots(fix, parent)
    partial_(departial_(`__f`) %||% f, fmls, fix, parent)
  }
})

fn_template <- function(fmls)
  eval(call("function", fmls, NULL))

fn_template_partial <- fn_template(formals(partial))

call_quos_match <- function(dots, fmls) {
  call <- match.call(fn_template(fmls), dots)
  call[[1]] <- quos
  call
}

name_bare_dots <- function(xs, env) {
  nms <- names(xs)
  nms_prev <- names_fixed_args(env)
  is_bare_dot <- !nzchar(nms)
  n_bare_dots <- sum(is_bare_dot)
  n_prev_dots <- sum(is_bare_dot_name(nms_prev))
  # '__1', '__2', ... mimic names of elements of '...'
  nms[is_bare_dot] <- paste0("__", n_prev_dots + seq_len(n_bare_dots))
  all(nms %notin% nms_prev) %because% "Can't reset previously fixed argument(s)"
  nms
}

names_fixed_args <- getter("__names_fixed_args__", mode = "character")
`names_fixed_args<-` <- setter("__names_fixed_args__")

is_bare_dot_name <- function(nms)
  grepl("^__[[:digit:]]*$", nms)

partial_ <- function(fun, fmls, fix, parent) {
  env <- bind_fixed_args(fix, parent)
  env$`__fun__` <- fun
  fmls_fun <- formals(fun)
  dots(env) <- c(dots(parent), dot_args(fix, fmls_fun))
  args <- eponymous_args(fmls_fun, dots(env))
  fmls_trunc <- truncate(fmls, cut = fix)
  fn(!!! fmls_trunc, ~ `__fun__`(!!! args), ..env = env)
}

bind_fixed_args <- function(fix, parent) {
  env <- new.env(parent = parent)
  nms_fix <- names(fix)
  names_fixed_args(env) <- c(names_fixed_args(parent), nms_fix)
  for (nm in nms_fix)
    makeActiveBinding(nm, get_tidy(fix[[nm]]), env)
  env
}

get_tidy <- function(q) {
  force(q)
  function(.) eval_tidy(q)
}

dots <- getter("__dots__", mode = "list")
`dots<-` <- setter("__dots__")

dot_args <- function(fix, fmls) {
  nms_fix <- names(fix)
  nms_dots <- nms_fix[nms_fix %notin% names_nondots(fmls)]
  dots <- eponymous(nms_dots)
  names(dots)[is_bare_dot_name(nms_dots)] <- ""
  dots
}

truncate <- function(xs, cut) {
  nms_cut <- names(cut)
  xs[names(xs) %notin% nms_cut]
}

eponymous_args <- function(fmls, dots) {
  nms_fmls <- names(fmls)
  if ("..." %notin% names(fmls))
    return(eponymous(nms_fmls))
  c(eponymous(nondots(nms_fmls)), dots, quote(...))
}

#' @rdname partial
#' @export
departial <- function(`__f`) {
  is.function(`__f`) %because% "Only functions can be de-partialized"
  departial_(`__f`) %||% `__f`
}

departial_ <- fun_getter("__fun__")

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
#'   as [quosures][rlang::quotation]. [Unquoting][rlang::quasiquotation] and
#'   [splicing][rlang::quasiquotation] are supported (see _Examples_).
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
#' stopifnot(
#'   identical(
#'     formals(partial(foo)),
#'     formals(foo)
#'   ),
#'   identical(
#'     formals(partial(foo, x = 1)),
#'     formals(function(y = x, ..., z = "z") {})
#'   ),
#'   identical(
#'     formals(partial(foo, x = 1, y = 2)),
#'     formals(function(..., z = "z") {})
#'   ),
#'   identical(
#'     formals(partial(foo, x = 1, y = 2, z = 3)),
#'     formals(function(...) {})
#'   )
#' )
#'
#' @export
partial <- local({
  dots_match <- function(call, fmls) {
    call <- match.call(fn_template_partial, call)
    call <- call[names(call) != "__f"]
    match.call(fn_template(fmls), call)
  }
  quos_dots_match <- call_in_caller_env(quos, dots_match)
  partial <- function(`__f`, ...) {
    f <- closure(`__f`)
    fmls <- formals(f)
    fix <- quos_dots_match(fmls)  # '...' consumed by introspection
    if (is_empty(fix))
      return(`__f`)
    partial_(departial_(`__f`) %||% f, fmls, fix, environment(f))
  }
  fn_template <- function(fmls) new_function_(fmls, NULL)
  fn_template_partial <- fn_template(formals(partial))

  partial
})

partial_ <- local({
  body_partial <- quote({
    environment(`__partial__`) <- `__with_fixed_args__`()
    eval(`[[<-`(sys.call(), 1, `__partial__`), parent.frame())
  })
  call_bare     <- getter("__call_bare__")
  `call_bare<-` <- setter("__call_bare__")

  function(f_bare, fmls, fix, parent) {
    fmls_partial <- fmls[names(fmls) %notin% names(fix)]
    nms_bare <- names(formals(f_bare))
    if (has_dots(nms_bare)) {
      env <- bind_fixed_args(fix, parent, nondots(nms_bare))
      call_bare(env) <- as.call(c(quote(`__bare__`), args(env), quote(...)))
    } else {
      env <- bind_fixed_args(fix, parent)
      call_bare(env) <- call_bare(parent) %||%
        as.call(c(quote(`__bare__`), eponymous(nms_bare)))
    }
    env$`__bare__` <- f_bare
    env$`__partial__` <- new_function_(fmls_partial, call_bare(env), env)
    env$`__with_fixed_args__` <- promise_tidy(nms_bare, fmls_partial, env)
    new_function_(fmls_partial, body_partial, env)
  }
})

args        <- getter("__args__")
names_fixed <- getter("__names_fixed__")

bind_fixed_args <- local({
  `args<-`        <- setter("__args__")
  `names_fixed<-` <- setter("__names_fixed__")

  function(fix, parent, nms = NULL) {
    if (is.null(nms)) {
      names(fix) <- privatize(names(fix))
      return(list2env(fix, parent = parent))
    }
    all(names(fix) %notin% names_fixed(parent)) %because%
      "Can't reset previously fixed argument(s)"
    names(fix) <- name_bare_dots(fix, parent)
    env <- bind_fixed_args_(fix, nms, parent)
    args(env) <- c(args(parent) %||% eponymous(nms), tidy_dots(names(fix), nms))
    names_fixed(env) <- c(names_fixed(parent), names(fix))
    env
  }
})

bind_fixed_args_ <- function(fix, nms, parent) {
  is_nondot <- names(fix) %in% nms
  names(fix)[is_nondot] <- privatize(names(fix)[is_nondot])
  list2env(fix, parent = parent)
}

name_bare_dots <- function(xs, env) {
  nms <- names(xs)
  is_bare_dot <- !nzchar(nms)
  n_bare_dots <- sum(is_bare_dot)
  if (n_bare_dots == 0)
    return(nms)
  n_prev_dots <- sum(is_bare_dot_name(names_fixed(env)))
  nms[is_bare_dot] <- privatize(n_prev_dots + seq_len(n_bare_dots))
  nms
}

# Bare dot names have the form '..%d..' for %d >= 1
is_bare_dot_name <- function(nms) {
  grepl("^\\.\\.[123456789][[:digit:]]*\\.\\.$", nms)
}

tidy_dots <- function(nms_fix, nms_nondots) {
  nms_dots <- nms_fix[nms_fix %notin% nms_nondots]
  dots <- map_eval_tidy(nms_dots)
  names(dots)[is_bare_dot_name(nms_dots)] <- ""
  dots
}

promise_tidy <- function(nms, exclude, parent) {
  nms <- nms[nms %notin% names(exclude)]
  promises <- map_eval_tidy(privatize(nms), nms)
  env <- list2env(list(eval_tidy = eval_tidy), parent = parent)
  new_function_(promises, quote(environment()), env)
}

map_eval_tidy <- function(nms, rename = nms) {
  names(nms) <- rename
  lapply(nms, function(nm) call("eval_tidy", as.name(nm)))
}

privatize <- function(nms) sprintf("..%s..", nms)

#' @rdname partial
#' @export
departial <- function(`__f`) {
  is.function(`__f`) %because% "Only functions can be de-partialized"
  departial_(`__f`) %||% `__f`
}

departial_ <- getter("__bare__", environment)

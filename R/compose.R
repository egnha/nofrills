#' Compose functions
#'
#' @description
#' Compose functions in three ways:
#'
#' - Using `compose()`: `compose(f, g)` is the function that calls `g` followed
#'   by `f`. It has the [formals][base::formals()] of `g`.
#'
#' - Using \code{\%<<<\%} (\dQuote{backward} composition): \code{f \%<<<\% g}
#'   is another way to express `compose(f, g)`.
#'
#' - Using \code{\%>>>\%} (\dQuote{forward} composition): \code{f \%>>>\% g}
#'   is another way to express `compose(g, f)`.
#'
#' Use `decompose()` to recover the list of composite functions of a function
#' composition.
#'
#' @param ... Functions or lists thereof to compose. Lists of functions are
#'   automatically spliced in. (Explicit [splicing][rlang::quasiquotation] via
#'   `!!!` is also supported.) Following convention, functions are composed from
#'   right to left.
#' @param fst,snd,f Functions.
#'
#' @return `compose()`, \code{\%<<<\%} and \code{\%>>>\%} return a function
#'   composition, whose [formals][base::formals()] match those of the initial
#'   function called (as a closure).
#'
#'   `decompose()` returns the list of composite functions of a function
#'   composition (in reverse calling order), and wraps a non-composite function
#'   in a list.
#'
#' @section Properties: `compose()` is _associative_, semantically and
#'   operationally. This means, for instance, that
#'   `compose(f, g, h)`,
#'   `compose(f, compose(g, h))`,
#'   `compose(compose(f, g), h)`,
#'   are implemented as the _same function_. In other words, lists of functions
#'   are automatically \dQuote{flattened out} when they are composed, so nested
#'   compositions do not pile up.
#'
#'   `decompose()` and `compose()` are _mutually invertible_.
#'   `compose(decompose(f))` is the same as `f`, when `f` is a function.
#'   `decompose(compose(fs))` is the same as `fs`, when `fs` is a list of
#'   functions.
#'
#' @examples
#' # Functions are composed from right to left (following convention)
#' inv <- curry(`/`)(1)  # reciprocal
#' f <- compose(inv, log, abs)
#' stopifnot(isTRUE(all.equal(f(-2), 1 / log(abs(-2)))))
#'
#' # "Backward" composition operator composes from right to left, like compose()
#' f1 <- inv %<<<% log %<<<% abs
#' stopifnot(isTRUE(all.equal(f1(-2), f(-2))))
#'
#' # Forward composition operator composes from left to right
#' f2 <- abs %>>>% log %>>>% inv
#' stopifnot(isTRUE(all.equal(f2(-2), f(-2))))
#'
#' # Curry composition to create input/output transformers
#' transform_in  <- curry(`%>>>%`)
#' transform_out <- curry(`%<<<%`)
#'
#' # Presume to_json()/from_json() convert to/from JSON
#' json_out <- transform_out(to_json)   # transforms function to produce JSON
#' json_in  <- transform_in(from_json)  # transforms function to consume JSON
#' jsonify  <- json_in %>>>% json_out   # transforms function to JSON function
#'
#' # Formals of initial function are preserved
#' first <- function(a, b = 0) a + b
#' stopifnot(identical(formals(compose(inv, first)), formals(first)))
#'
#' # Compositions can be provided by lists, in several equivalent ways
#' f3 <- compose(list(inv, log, abs))
#' f4 <- compose(!!! list(inv, log, abs))
#' f5 <- compose(inv, list(log, abs))
#' f6 <- compose(inv, !!! list(log, abs))
#' stopifnot(
#'   isTRUE(all.equal(f3, f)), isTRUE(all.equal(f3(-2), f(-2))),
#'   isTRUE(all.equal(f4, f)), isTRUE(all.equal(f4(-2), f(-2))),
#'   isTRUE(all.equal(f5, f)), isTRUE(all.equal(f5(-2), f(-2))),
#'   isTRUE(all.equal(f6, f)), isTRUE(all.equal(f6(-2), f(-2)))
#' )
#'
#' # compose() and decompose() are mutally invertible
#' f7 <- compose(inv, decompose(compose(log, abs)))
#' stopifnot(isTRUE(all.equal(f7, f)), isTRUE(all.equal(f7(-2), f(-2))))
#' fs <- list(inv, log, abs)
#' stopifnot(isTRUE(all.equal(decompose(compose(fs)), fs)))
#'
#' @export
compose <- local({
  iterated_call <- function(n, fmls) {
    fnames <- enum(n:1)
    expr <- as.call(c(as.name(fnames[[1]]), args(fmls)))
    for (fname in fnames[-1])
      expr <- call(fname, expr)
    expr
  }
  args <- function(fmls) {
    args <- eponymous(names(fmls))
    names(args)[names(args) == "..."] <- ""
    args
  }
  enum <- function(x) sprintf("__%s__", x)

  flatten_fns <- function(...) {
    fns <- unlist(lapply(list2(...), decompose_))
    are_funs(fns) %because% "Only functions or lists thereof can be composed"
    fns
  }
  are_funs <- function(xs) {
    !is_empty(xs) && all(vapply(xs, is.function, logical(1)))
  }

  get_pipeline <- function(pipeline, env) {
    force(env)
    nms <- names(pipeline)
    function(.)
      unname(mget(nms, envir = env, mode = "function", inherits = FALSE))
  }

  function(...) {
    pipeline <- flatten_fns(...)
    n <- length(pipeline)
    if (n == 1)
      return(pipeline[[1]])
    fn_init <- closure(pipeline[[n]])
    fmls <- formals(fn_init)
    body <- iterated_call(n, fmls)
    names(pipeline) <- enum(seq_len(n))
    env <- environment(fn_init) %encloses% pipeline
    makeActiveBinding("__pipeline__", get_pipeline(pipeline, env), env)
    fn_cmps <- new_fn(fmls, body, env)
    class(fn_cmps) <- c("CompositeFunction", "function")
    fn_cmps
  }
})

#' @rdname compose
#' @export
`%<<<%` <- function(snd, fst) {
  compose_implicit_partial(parent.frame(), substitute(snd), substitute(fst))
}

#' @rdname compose
#' @export
`%>>>%` <- opposite(`%<<<%`)

compose_implicit_partial <- local({
  implicit_partial <- function(expr, env) {
    if (is_literal(expr))
      return(eval(expr, env))
    call_partially <- as.call(c(partial, as.list(expr)))
    eval(call_partially, env)
  }

  is_literal <- function(expr) {
    !is.call(expr) || is_composition(expr) || is_paren(expr) || is_curly(expr)
  }
  is_composition <- function(call) {
    is_forward_compose(call) || is_backward_compose(call)
  }
  is_forward_compose  <- check_head("%>>>%")
  is_backward_compose <- check_head("%<<<%")
  is_paren <- check_head("(")
  is_curly <- check_head("{")

  function(env, ...) {
    fns <- lapply(list(...), implicit_partial, env = env)
    do.call("compose", fns)
  }
})

#' @rdname compose
#' @export
decompose <- function(f) {
  is.function(f) %because% "Only functions can be decomposed"
  box(decompose_(f))
}

decompose_ <- local({
  pipeline <- getter_env("__pipeline__")
  function(x) pipeline(x) %||% x
})

#' @export
print.CompositeFunction <- function(x, ...) {
  fns <- rev(decompose(x))
  cat("Composition of functions, listed in calling order:\n")
  for (i in seq_along(fns)) {
    cat("\n", i, ": ", sep = "")
    print(fns[[i]])
  }
  cat("\n(Use 'decompose()' to recover the list of composite functions.)")
  invisible(x)
}

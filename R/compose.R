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
compose <- function(...) {
  `__pipeline__` <- flatten_fns()  # '...' consumed via call introspection
  n <- length(`__pipeline__`)
  if (n == 1)
    return(`__pipeline__`[[1]])
  fn_last <- `__pipeline__`[[n]]
  `__call_fn_last__` <- function() {
    call <- `[[<-`(sys.call(-1), 1, fn_last)
    eval(call, parent.frame(2))
  }
  `__fns_rest__` <- rev(`__pipeline__`[-n])
  fn_comp <- function() {
    out <- `__call_fn_last__`()
    for (f in `__fns_rest__`)
      out <- f(out)
    out
  }
  formals(fn_comp) <- formals(as_closure(fn_last))
  class(fn_comp) <- c("CompositeFunction", "function")
  fn_comp
}

flatten_fns <- local({
  flatten <- list(
    compose = function(...) unlist(list2(...)),
    `%<<<%` = function(snd, fst) unlist(list2(snd, fst)),
    `%>>>%` = function(fst, snd) unlist(list2(snd, fst)),
    decompose = identity
  )
  are_funcs <- function(xs)
    !is_empty(xs) && all(vapply(xs, is.function, logical(1)))

  function() {
    dots <- eval(sys.call(-1), flatten, parent.frame(2))
    fns <- unlist(lapply(dots, decompose_))
    are_funcs(fns) %because% "Only functions or lists thereof can be composed"
    fns
  }
})

decompose_ <- function(x)
  environment(x)$`__pipeline__` %||% x

#' @param f,g Functions.
#' @rdname compose
#' @export
`%<<<%` <- function(f, g) compose(f, g)

#' @rdname compose
#' @export
`%>>>%` <- opposite(`%<<<%`)

#' @rdname compose
#' @export
decompose <- function(f) {
  is.function(f) %because% "Only functions can be decomposed"
  box(decompose_(f))
}

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

#' Compose functions
#'
#' @param ... Functions or lists thereof to compose. (Lists of functions are
#'   automatically spliced in; explicit [splicing][rlang::quasiquotation] via
#'   `!!!` is also supported.) Following convention, functions are composed from
#'   right to left.
#'
#' @return `compose()`, \code{\%<<<\%} and \code{\%>>>\%} return a function
#'   composition, whose [formals][formals()] match those of the initial function
#'   called. `decompose()` returns the list of composite functions of a function
#'   composition, and wraps a non-composite function in a list.
#'
#'
#' @export
compose <- function(...) {
  `__fns_composite` <- flatten_fns()  # '...' consumed via call introspection
  n <- length(`__fns_composite`)
  if (n == 1)
    return(`__fns_composite`[[1]])
  fn_last <- as_closure(`__fns_composite`[[n]])
  `__call_fn_last` <- function() {
    call <- `[[<-`(sys.call(-1), 1, fn_last)
    eval(call, parent.frame(2))
  }
  `__fns_rest` <- rev(`__fns_composite`[-n])
  fn_comp <- function() {
    out <- `__call_fn_last`()
    for (f in `__fns_rest`)
      out <- f(out)
    out
  }
  formals(fn_comp) <- formals(fn_last)
  structure(fn_comp, class = c("CompositeFunction", "function"))
}

flatten_fns <- local({
  flatten <- list(
    compose = function(...) unlist(list2(...)),
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
  environment(x)$`__fns_composite` %||% x

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
  wrap(decompose_(f))
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

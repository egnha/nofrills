#' Compose functions
#'
#' @param ... Functions to compose. [Splicing][rlang::quasiquotation] of a list
#'   of functions is supported.
#'
#' @return Composition of functions. (`NULL` is returned when no functions are
#'   given.)
#'
#' @export
compose <- function(...) {
  `__fns_composite` <- fns(...)
  if (length(`__fns_composite`) <= 1)
    return(`__fns_composite`[[1]])
  n <- length(`__fns_composite`)
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

#' @param f,g Functions.
#' @rdname compose
#' @export
`%<<<%` <- function(f, g) compose(f, g)

#' @rdname compose
#' @export
`%>>>%` <- opposite(`%<<<%`)

fns <- function(...) {
  fns <- lapply(dots_list(...), decompose)
  unlist(fns)
}

#' @rdname compose
#' @export
decompose <- function(f) {
  is.function(f) %because% "Only functions can be (de)composed"
  environment(f)$`__fns_composite` %||% f
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

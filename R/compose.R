#' Compose functions
#'
#' @param ... Functions to compose; lists of functions are automatically spliced
#'   in. (Explicit [splicing][rlang::quasiquotation] is supported, via `!!!`.)
#'
#' @return Composition of functions. (`NULL` is returned when no functions are
#'   given.)
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
  compose_as_list2 <- list(compose = list2)
  is_function_list <- function(xs)
    !is_empty(xs) && all(vapply(xs, is.function, logical(1)))

  function() {
    dots <- eval(sys.call(-1), compose_as_list2, parent.frame(2))
    fns <- unlist(lapply(dots, decompose_))
    is_function_list(fns) %because% "Only functions can be composed"
    fns
  }
})

decompose_ <- function(f)
  environment(f)$`__fns_composite` %||% f

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
  decompose_(f)
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

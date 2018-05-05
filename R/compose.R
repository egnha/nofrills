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
#' inv <- partial(`/`, 1)  # reciprocal
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
#' # Presume to_json()/from_json() convert to/from JSON
#' \dontrun{
#' json_out <- partial(`%<<<%`, to_json)    # transforms function to produce JSON
#' json_in  <- partial(`%>>>%`, from_json)  # transforms function to consume JSON
#' jsonify  <- json_in %>>>% json_out       # transforms function to JSON function
#' }
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
  pipeline <- flatten_fns(...)
  n <- length(pipeline)
  if (n == 0L)
    return(identity)
  if (n == 1L)
    return(pipeline[[1L]])
  fn_init <- closure(pipeline[[n]])
  fmls <- formals(fn_init)
  call <- iterated_call(n, fmls)
  fnms <- call$fnms
  env <- environment(fn_init) %encloses% (pipeline %named% fnms)
  makeActiveBinding("__pipeline__", get_fns(fnms, names(pipeline), env), env)
  fn_cmps <- new_fn(fmls, call$expr, env)
  class(fn_cmps) <- c("CompositeFunction", "function")
  fn_cmps
}

flatten_fns <- function(...) {
  fns <- lapply(list_tidy(...), fn_interp)
  unlist(do.call(c, fns))  # Collapse NULL's by invoking 'c'
}

fn_interp <- function(x) {
  UseMethod("fn_interp")
}

#' @export
fn_interp.quosure <- function(x) {
  expr <- quo_get_expr(x)
  if (!is.call(expr) || is_compose_op(expr))
    return(fn_interp(eval_tidy(x)))
  if (is_named(expr))
    return(lambda_named(expr, quo_get_env(x)))
  if (is_lambda(expr))
    return(lambda(expr, quo_get_env(x)))
  lambda_partial(expr, quo_get_env(x))
}

is_compose_op <- function(expr) {
  is_forward_compose(expr) || is_backward_compose(expr)
}
is_forward_compose  <- check_head("%>>>%")
is_backward_compose <- check_head("%<<<%")

lambda_named <- function(expr, env) {
  expr[[1L]] <- quote(`:=`)
  enquos <- as.call(c(quos, expr))
  fn_interp(eval(enquos, env))
}
is_named <- check_head(":")

lambda <- function(body, env) {
  new_fn(alist(. = ), body, env)
}
is_lambda <- check_head("{")

lambda_partial <- local({
  is_void <- function(call) length(call) == 1L
  placeholder <- as.name(".")
  validate <- function(call, env) {
    match_call <- tryCatch(
      {
        f <- get(as.character(call[[1L]]), envir = env, mode = "function")
        match.call(args(f), call)
      },
      error = function(.) .$message
    )
    if (is.character(match_call))
      halt("%s is an invalid call: %s", expr_label(call), match_call)
    invisible(call)
  }

  function(call, env) {
    if (is_void(call)) {
      f <- eval(call[[1L]], env)
      if (!is.function(f))
        halt("Expected %s to be a function", expr_label(call[[1L]]))
      return(f)
    }
    args <- as.list(call)[-1L]
    if (all(args != placeholder))
      call <- as.call(c(call[[1L]], quote(.), args))
    validate(call, env)
    lambda(call, env)
  }
})

#' @export
fn_interp.quosures <- function(x) {
  lapply(x, fn_interp.quosure)
}

#' @export
fn_interp.list <- function(x) {
  lapply(x, fn_interp)
}

#' @export
fn_interp.CompositeFunction <- function(x) {
  .subset2(environment(x), "__pipeline__")
}

#' @export
fn_interp.function <- function(x) {
  if (identical(x, identity))
    return(NULL)
  x
}

#' @export
fn_interp.NULL <- function(x) NULL

#' @export
fn_interp.default <- function(x) {
  cls <- paste(deparse(class(x)), collapse = "")
  halt("Cannot interpret object of class %s as a function", cls)
}

iterated_call <- local({
  args <- function(fmls) {
    args <- eponymous(names(fmls))
    names(args)[names(args) == "..."] <- ""
    args
  }

  function(n, fmls) {
    fnms <- sprintf("__%s__", n:1L)
    expr <- as.call(c(as.name(fnms[[1L]]), args(fmls)))
    for (nm in fnms[-1L])
      expr <- call(nm, expr)
    list(expr = expr, fnms = rev(fnms))
  }
})

get_fns <- function(fnms, nms, env) {
  force(fnms)
  force(nms)
  force(env)

  function() {
    fns <- mget(fnms, envir = env, mode = "function", inherits = FALSE)
    fns %named% nms
  }
}

#' @param fst,snd Functions.
#' @rdname compose
#' @export
`%>>>%` <- function(fst, snd) {
  compose(enquo(snd), enquo(fst))
}

#' @rdname compose
#' @export
`%<<<%` <- opposite(`%>>>%`)

#' @param f Function.
#' @rdname compose
#' @export
decompose <- local({
  pipeline <- getter("__pipeline__")
  function(f) {
    is.function(f) %because% "Only functions can be decomposed"
    box(pipeline(f) %||% f)
  }
})

#' @export
print.CompositeFunction <- function(x, ...) {
  cat("<Function Composition (in calling order)>\n")
  fns <- rev(decompose(x))
  nms <- names_chr(fns)
  nms[!nzchar(nms)] <- list(NULL)
  for (i in seq_along(fns)) {
    out <- c(sprintf("$%s", nms[[i]]), trim_capture(fns[[i]]))
    pad <- c(sprintf("%2d.\ ", i), rep("\ \ \ \ ", length(out) - 1L))
    cat("\n", paste0(pad, out, "\n"), sep = "")
  }
  cat("\nRecover the list of functions with 'decompose()'.")
  invisible(x)
}

#' @importFrom utils capture.output
trim_capture <- function(f) {
  out <- capture.output(print(f))
  if (inherits(f, c("PartialFunction", "TidyFunction")))
    out <- out[-c(2L, length(out) - 1L, length(out))]
  out
}

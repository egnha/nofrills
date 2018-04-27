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
compose <- local({
  iterated_call <- function(n, fmls) {
    fnames <- sprintf("__%s__", n:1L)
    expr <- as.call(c(as.name(fnames[[1L]]), args(fmls)))
    for (fname in fnames[-1L])
      expr <- call(fname, expr)
    list(expr = expr, fnames = rev(fnames))
  }
  args <- function(fmls) {
    args <- eponymous(names(fmls))
    names(args)[names(args) == "..."] <- ""
    args
  }

  flatten_fns <- function(..., ..env) {
    fns <- lapply(list_tidy(...), fn_interp, env = ..env)
    unlist(do.call(c, fns))  # Collapse NULL's by invoking 'c'
  }

  get_pipeline <- function(pipeline, env) {
    force(env)
    nms <- names(pipeline)
    function(.) {
      unname(mget(nms, envir = env, mode = "function", inherits = FALSE))
    }
  }

  function(...) {
    pipeline <- flatten_fns(..., ..env = parent.frame())
    n <- length(pipeline)
    (n > 0L) %because% "Must specify functions to compose"
    if (n == 1L)
      return(pipeline[[1L]])
    fn_init <- closure(pipeline[[n]])
    fmls <- formals(fn_init)
    call <- iterated_call(n, fmls)
    names(pipeline) <- call$fnames
    env <- environment(fn_init) %encloses% pipeline
    makeActiveBinding("__pipeline__", get_pipeline(pipeline, env), env)
    fn_cmps <- new_fn(fmls, call$expr, env)
    class(fn_cmps) <- c("CompositeFunction", "function")
    fn_cmps
  }
})

fn_interp <- function(x, ...) {
  UseMethod("fn_interp")
}

#' @export
fn_interp.list <- function(x, ...) {
  lapply(x, fn_interp, ...)
}

#' @export
fn_interp.CompositeFunction <- function(x, ...) {
  .subset2(environment(x), "__pipeline__")
}

#' @export
fn_interp.function <- function(x, ...) x

#' @export
fn_interp.formula <- function(x, ...) {
  is_onesided(x) %because% "Lifted function must be a one-sided formula"
  lift(f_rhs(x), environment(x))
}
lift <- function(expr, env) {
  f <- eval(expr, env)
  is.function(f) %because% "Only functions can be lifted"
  pipeline <- fn_interp(f)
  if (!inherits(f, "CompositeFunction"))
    return(lift_(pipeline, expr_name(expr)))
  n <- length(pipeline)
  pipeline[[n]] <- lift_(pipeline[[n]])
  pipeline
}
lift_ <- function(f, nm = expr_name(f)) {
  body <- bquote(do.call(.(nm), args))
  new_fn(alist(args = ), body, baseenv(), !!nm := f)
}

#' @export
fn_interp.logical <- function(x, env) {
  len <- length(x)
  if (len == 0L)
    return(NULL)
  msg <- sprintf("Filter length (%d) must equal input length (%%d)", len)
  nms <- names_chr(x)[x]
  rename <- nzchar(nms)
  if (any(rename)) {
    body <- expr({
      if (length(x) != !!len) stop(sprintf(!!msg, length(x)), call. = FALSE)
      x <- x[!!x]
      names(x) <- names_chr(x)
      names(x)[!!rename] <- !!(nms[rename])
      x
    })
  } else {
    body <- expr({
      if (length(x) != !!len) stop(sprintf(!!msg, length(x)), call. = FALSE)
      x[!!x]
    })
  }
  # Setting the environment allows the implied `[` method to be found (#37)
  new_fn(alist(x = ), body, env, names_chr = names_chr)
}

#' @export
fn_interp.character <- function(x, env) {
  if (length(x) == 0L)
    return(NULL)
  nms <- names_chr(x)
  rename <- nzchar(nms)
  if (any(rename)) {
    body <- expr({
      x <- x[!!x]
      names(x) <- names_chr(x)
      names(x)[!!rename] <- !!(nms[rename])
      x
    })
  } else {
    body <- expr(x[!!x])
  }
  new_fn(alist(x = ), body, env, names_chr = names_chr)
}

#' @export
fn_interp.integer <- fn_interp.character

#' @export
fn_interp.numeric <- fn_interp.character

#' @export
fn_interp.NULL <- function(x, ...) NULL

#' @export
fn_interp.default <- function(x, ...) {
  cls <- paste(deparse(class(x)), collapse = "")
  msg <- sprintf("Cannot interpret object of class %s as a function", cls)
  stop(msg, call. = FALSE)
}

#' @param fst,snd Functions.
#' @rdname compose
#' @export
`%>>>%` <- function(fst, snd) compose(snd, fst)

#' @rdname compose
#' @export
`%<<<%` <- opposite(`%>>>%`)

#' @param f Function.
#' @rdname compose
#' @export
decompose <- local({
  pipeline <- getter_env("__pipeline__")
  function(f) {
    is.function(f) %because% "Only functions can be decomposed"
    box(pipeline(f) %||% f)
  }
})

#' @export
print.CompositeFunction <- function(x, ...) {
  cat("<Function Composition (in calling order)>\n")
  fns <- rev(decompose(x))
  for (i in seq_along(fns)) {
    out <- trim_capture(fns[[i]])
    pad <- c(sprintf("%2d:\ ", i), rep("\ \ \ \ ", length(out) - 1L))
    cat("\n", paste0(pad, out, "\n"), sep = "")
  }
  cat("\nRecover the list of functions with 'decompose()'.")
  invisible(x)
}

#' @importFrom utils capture.output
trim_capture <- function(f) {
  out <- capture.output(print(f))
  if (inherits(f, c("CurriedFunction", "PartialFunction", "TidyFunction")))
    out <- out[-c(2L, length(out) - 1L, length(out))]
  out
}

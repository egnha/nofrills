#' Compose functions
#'
#' @description
#' Compose functions in two ways:
#'
#' - Use `compose(f, g, ...)` to make the function that applies `f`, then `g`,
#'   etc. It has the [formals][base::formals()] of the “inner” function `f`.
#'   Thus
#'   \preformatted{compose(paste, toupper)}
#'   is equivalent to the function
#'   ```
#'   function(..., sep = " ", collapse = NULL) {
#'     toupper(paste(..., sep = sep, collapse = collapse))
#'   }
#'   ```
#'
#' - Alternatively, use the infix notation \code{f \%>>>\% g \%>>>\% ...}, which
#'   comprehends the semantics of the
#'   [\pkg{magrittr}](https://cran.r-project.org/package=magrittr)-\code{\%>\%}
#'   operator and, additionally, [quasiquotation][rlang::quasiquotation].
#'   Thus, assuming `sep` has the value `""`,
#'   \preformatted{sample \%>>>\% paste(collapse = !!sep)}
#'   is equivalent to the function
#'   ```
#'   function(x, size, replace = FALSE, prob = NULL) {
#'     paste(sample(x, size, replace, prob), collapse = "")
#'   }
#'   ```
#'
#' Use `as.list()` to recover the list of composite functions.
#'
#' @param ... Functions or lists thereof to compose. Lists of functions are
#'   automatically spliced in. [Unquoting][rlang::quasiquotation] of names, via
#'   `!!` on the left-hand side of `:=`, and [splicing][rlang::quasiquotation],
#'   via `!!!`, are supported.
#'
#' @return `compose()` returns a function of class `CompositeFunction`, whose
#'   [formals][base::formals()] match those of the inner function applied (as a
#'   closure).
#'
#' @section Properties: `compose()` is _associative_, semantically and
#'   operationally. This means, for instance, that
#'   `compose(f, g, h)`,
#'   `compose(f, compose(g, h))`,
#'   `compose(compose(f, g), h)`,
#'   are implemented as the _same function_. In other words, lists of functions
#'   are automatically “flattened out” when they are composed—intermediate
#'   compositions are spliced rather than nested.
#'
#'   `as.list()` and `compose()` are _mutually invertible_.
#'   `as.list(compose(fs))` is the same as `fs`, when `fs` is a list of
#'   functions (though the names of `as.list()` are always strings).
#'
#' @examples
#' # Functions are composed from right to left (following convention)
#' inv <- partial(`/`, 1)  # reciprocal
#' f0 <- compose(abs, log, inv)
#' stopifnot(all.equal(f0(-2), 1 / log(abs(-2))))
#'
#' # Forward composition operator composes from left to right
#' f1 <- abs %>>>% log %>>>% {1 / .}
#' stopifnot(all.equal(f1(-2), f0(-2)))
#'
#' # Compose higher-order functions
#' \dontrun{
#' # Transforms function to a JSON function
#' require(jsonlite)
#' jsonify <- {fromJSON %>>>% .} %>>>% {. %>>>% toJSON}
#' jsonify <- fn(f ~ fromJSON %>>>% f %>>>% toJSON)}
#'
#' # Formals of initial function are preserved
#' inner <- function(a, b = 0) a + b
#' stopifnot(identical(formals(compose(inner, inv)), formals(inner)))
#'
#' # Compositions can be provided by lists, in several equivalent ways
#' f2 <- compose(list(abs, log, inv))
#' f3 <- compose(!!! list(abs, log, inv))
#' f4 <- compose(abs, list(log, inv))
#' f5 <- compose(abs, !!! list(log, inv))
#' stopifnot(
#'   all.equal(f2, f0), all.equal(f2(-2), f0(-2)),
#'   all.equal(f3, f0), all.equal(f3(-2), f0(-2)),
#'   all.equal(f4, f0), all.equal(f4(-2), f0(-2)),
#'   all.equal(f5, f0), all.equal(f5(-2), f0(-2))
#' )
#'
#' # compose() and as.list() are mutally invertible
#' f6 <- compose(abs, as.list(compose(log, inv)))
#' stopifnot(
#'   all.equal(f6, f0), all.equal(f6(-2), f0(-2))
#' )
#' fs <- list(abs, log, inv)
#' stopifnot(all.equal(check.attributes = FALSE,
#'   as.list(compose(fs)), fs,
#' ))
#'
#' # `%>>>%` supports names, magrittr-`%>%` semantics, quasiquotation
#' sep <- ""
#' scramble <- shuffle: sample %>>>% paste(collapse = !!sep)
#' nonsense <- scramble(letters)
#' stopifnot(
#'   nchar(nonsense) == 26L,
#'   identical(letters, sort(strsplit(nonsense, sep)[[1]])),
#'   identical(scramble$shuffle, sample)
#' )
#'
#' @export
compose <- function(...) {
  pipeline <- flatten_pipeline(...)
  if (is_empty(pipeline))
    return(NULL)
  cmp <- fuse(pipeline)
  class(cmp) <- c("CompositeFunction", "function")
  cmp
}

fuse <- function(pipeline) {
  innermost <- pipeline[[1L]]
  fmls <- fml_args(innermost)
  call <- nest_calls(length(pipeline), fmls)
  env <- envir(innermost) %encloses% (pipeline %named% call$fnms)
  makeActiveBinding("__pipeline__", get_fns(call$fnms, pipeline, env), env)
  new_fn(fmls, call$expr, env)
}

nest_calls <- function(n, fmls) {
  fnms <- fmt("__%s__", seq_len(n))
  args <- lapply(names(fmls), as.name)
  expr <- as.call(c(as.name(fnms[[1L]]), args))
  for (nm in fnms[-1L])
    expr <- call(nm, expr)
  list(expr = expr, fnms = fnms)
}

flatten_pipeline <- function(...) {
  fns <- lapply(list_tidy(...), fn_interp)
  unlist(do.call(c, fns))  # Collapse NULL's by invoking 'c'
}

fn_interp <- function(x) {
  UseMethod("fn_interp")
}

#' @export
fn_interp.quosure <- function(x) {
  expr <- quo_get_expr(x)
  if (!is.call(expr) || is_literal(expr))
    return(fn_interp(eval_tidy(x)))
  if (is_named(expr))
    return(lambda_named(expr, quo_get_env(x)))
  if (is_lambda(expr))
    return(lambda(expr, quo_get_env(x)))
  lambda_partial(expr, quo_get_env(x))
}

is_literal <- function(expr) {
  is_op_compose(expr) || is_paren(expr) || is_op_namespace(expr)
}
is_op_compose <- check_head("%>>>%")
is_paren      <- check_head("(")
is_op_namespace <- function(expr) {
  is_op_public(expr) || is_op_private(expr)
}
is_op_public  <- check_head("::")
is_op_private <- check_head(":::")

is_named <- check_head(":")
lambda_named <- function(expr, env) {
  expr[[1L]] <- quote(`:=`)
  enquos <- as.call(c(quos, expr))
  fn_interp(eval(enquos, env))
}

is_lambda <- check_head("{")
lambda <- function(body, env) {
  new_fn(alist(. = ), body, env)
}

lambda_partial <- local({
  point <- as.name(".")
  is_void <- function(call) {
    length(call) == 1L
  }
  conform <- function(call, to) {
    call_conform <- match.call(args(to) %||% as_closure(to), call) %unless%
      fmt("%s is an invalid call: %%s", expr_label(call))
    if (is.primitive(to))
      return(call)
    call_conform
  }

  function(call, env) {
    caller <- match.fun(eval_tidy(call[[1L]], env))
    if (is_void(call))
      return(caller)
    args <- as.list(call)[-1L]
    if (all(args != point))
      call <- as.call(c(call[[1L]], point, args))
    call <- conform(call, to = caller)
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
fn_interp.CompositeFunction <- getter("__pipeline__")

#' @export
fn_interp.function <- function(x) x

#' @export
fn_interp.NULL <- function(x) NULL

#' @export
fn_interp.default <- function(x) {
  cls <- paste(deparse(class(x)), collapse = "")
  halt("Cannot interpret object of class %s as a function", cls)
}

get_fns <- function(fnms, fns, env) {
  force(fnms)
  force(env)
  nms <- names_chr(fns)

  function() {
    fns <- mget(fnms, envir = env, mode = "function", inherits = FALSE)
    fns %named% nms
  }
}

#' @param inner,outer Functions. These may be optionally named using `:`, e.g.,
#'   \code{f \%>>>\% nm: g} names the `g`-component.
#'   [Quasiquotation][rlang::quasiquotation] and the
#'   [\pkg{magrittr}](https://cran.r-project.org/package=magrittr)-\code{\%>\%}
#'   semantics are supported (see _Examples_).
#'
#' @rdname compose
#' @export
`%>>>%` <- function(inner, outer) {
  compose(enquo(inner), enquo(outer))
}

#' @export
`$.CompositeFunction` <- function(x, i) {
  fns <- as.list.CompositeFunction(x)
  .subset2(fns, i)
}
#' @export
`$<-.CompositeFunction` <- function(x, name, value) {
  fns <- as.list.CompositeFunction(x)
  fns[[name]] <- value
  compose(fns)
}

#' @export
`[[.CompositeFunction` <- function(x, i, ...) {
  fns <- as.list.CompositeFunction(x)
  .subset2(fns, i)
}
#' @export
`[[<-.CompositeFunction` <- function(x, i, value) {
  fns <- as.list.CompositeFunction(x)
  fns[[i]] <- value
  compose(fns)
}

#' @export
`[.CompositeFunction` <- function(x, i) {
  if (missing(i))
    return(x)
  fns <- as.list.CompositeFunction(x)
  if (is.numeric(i))
    i <- i[abs(i) <= length(fns)]
  if (is.logical(i) && length(i) != length(fns))
    halt("Length of predicate (%d) must equal length of composition (%d)",
         length(i), length(fns))
  compose(.subset(fns, i))
}

#' @export
names.CompositeFunction <- function(x) {
  names(as.list.CompositeFunction(x))
}
#' @export
`names<-.CompositeFunction` <- function(x, value) {
  fns <- as.list.CompositeFunction(x)
  # From rlang::names2()
  if (is.null(value)) {
    value <- rep("", length(fns))
  } else {
    value <- value %|% ""
  }
  names(fns) <- value
  compose(fns)
}

#' @export
length.CompositeFunction <- function(x) {
  length(as.list.CompositeFunction(x))
}

#' @export
as.list.CompositeFunction <- function(x, ...) {
  fn_interp.CompositeFunction(x)
}

#' @export
print.CompositeFunction <- function(x, ...) {
  cat("<Function Composition>\n")
  cat("From the inner to outer function:\n")
  fns <- as.list.CompositeFunction(x)
  nms <- names_chr(fns)
  nms[!nzchar(nms)] <- list(NULL)
  for (i in seq_along(fns)) {
    out <- c(fmt("$%s", nms[[i]]), trim_capture(fns[[i]]))
    pad <- c(fmt("%2d.\ ", i), rep("\ \ \ \ ", length(out) - 1L))
    cat("\n", paste0(pad, out, "\n"), sep = "")
  }
  cat("\nRecover the list of functions with 'as.list()'.")
  invisible(x)
}

#' @importFrom utils capture.output
trim_capture <- function(f) {
  out <- capture.output(print(f))
  if (inherits(f, c("PartialFunction", "TidyFunction")))
    out <- out[-c(2L, length(out) - 1L, length(out))]
  out
}

#' @param f Function.
#'
#' @return For a composite function, `distill()` drops every component function
#'   that is identical to [base::identity()], yielding another composite
#'   function. Non-composite functions are returned unchanged.
#'
#' @examples
#' # distill() drops identity components
#' vals <- {set.seed(1); runif(10, 1, 2)}
#' sum_log <- log %>>>% identity %>>>% sum %>>>% identity
#' stopifnot(
#'   length(distill(sum_log)) == 2L,
#'   all.equal(sum_log(vals), sum(log(vals)))
#' )
#'
#' @rdname compose
#' @export
distill <- function(f) {
  f <- match.fun(f)
  if (!inherits(f, "CompositeFunction"))
    return(f)
  not_identity <- !vapply(f, identical, identity, FUN.VALUE = TRUE)
  f[not_identity] %||% compose(identity)
}

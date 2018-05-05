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
#' @param ..f Function.
#' @param ... Argument values of `..f` to fix, specified by name. Captured as
#'   [quosures][rlang::quotation]. [Unquoting][rlang::quasiquotation] and
#'   [splicing][rlang::quasiquotation] are supported (see _Examples_).
#'
#' @return `partial()` returns a function whose [formals][base::formals()] are a
#'   literal truncation of the formals of `..f()` (as a closure) by the fixed
#'   arguments. `partial(..f)` is identical to `..f`.
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
#' stopifnot(all(vapply(reps, length, integer(1)) == len))
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
  assign_setter("expr_partial")

  expr_fn <- function(..f, f) {
    expr <- substitute(..f, parent.frame())
    if (is.name(expr))
      return(expr)
    call("(", call("function", formals(f), quote(...)))
  }

  function(..f, ...) {
    if (missing(...))
      return(..f)
    f <- closure(..f)
    p <- partial_(f, ...)
    expr_partial(p) <- expr_partial(f) %||% expr_fn(..f, f)
    class(p) <- "PartialFunction" %subclass% class(..f)
    p
  }
})

assign_getter("expr_partial")
assign_getter("names_fixed")

partial_ <- local({
  assign_getter("bare_args")
  assign_setter("bare_args")
  assign_setter("names_fixed")

  body_partial <- quote({
    environment(`__partial__`) <- `__with_fixed_args__`()
    eval(`[[<-`(sys.call(), 1L, `__partial__`), parent.frame())
  })

  args <- function(f, nms) {
    bare_args(f) %||% eponymous(nms)
  }
  call_bare <- function(...) {
    as.call(c(quote(`__bare__`), ...))
  }

  no_name_reuse <- function(f, fix) {
    all(names(fix)[nzchar(names(fix))] %notin% names(names_fixed(f)))
  }

  function(f, ...) {
    fix <- quos_match(f, ...)
    f_bare <- departial_(f)
    nms_bare <- names(formals(f_bare))
    if (has_dots(nms_bare)) {
      no_name_reuse(f, fix) %because% "Can't reset previously fixed argument(s)"
      nms_bare <- nms_bare[nms_bare != "..."]
      nms_priv <- privatize(names(fix), names_fixed(f))
      args <- c(args(f, nms_bare), tidy_dots(nms_priv, nms_bare))
      body <- call_bare(args, quote(...))
    } else {
      nms_priv <- privatize(names(fix))
      args <- args(f, nms_bare)
      body <- call_bare(args)
    }
    nms_fix <- c(nms_priv, names_fixed(f))
    fmls_partial <- formals(f)[names(formals(f)) %notin% names(fix)]
    env <- environment(f) %encloses% (fix %named% nms_priv)
    env %binds% list(
      `__with_fixed_args__` = promise_tidy(nms_fix, nms_bare, env),
      `__partial__`         = new_fn(fmls_partial, body, env),
      `__bare__`            = f_bare
    )
    p <- new_fn(fmls_partial, body_partial, env)
    names_fixed(p) <- nms_fix
    bare_args(p)   <- args
    p
  }
})

quos_match <- local({
  non_void_expr <- function(q) {
    quo_get_expr(q) != quote(expr = )
  }

  function(..f, ...) {
    qs <- quos(...)
    ordered <- as.call(c(quote(c), seq_along(qs) %named% names(qs)))
    matched <- eval(match.call(..f, ordered), baseenv())
    qs <- qs %named% names_chr(matched)[order(matched)]
    qs[vapply(qs, non_void_expr, TRUE)]
  }
})

privatize <- local({
  privatize_ <- function(xs, nms = xs) {
    sprintf("..%s..", xs) %named% nms
  }
  n_dots <- function(x) {
    if (is.null(x))
      return(0L)
    sum(!nzchar(names(x)))
  }

  function(nms, nms_prev) {
    if (missing(nms_prev))
      return(privatize_(nms))
    nms_fill <- nms
    is_blank <- !nzchar(nms_fill)
    if ((n_blank <- sum(is_blank)) != 0L)
      nms_fill[is_blank] <- as.character(n_dots(nms_prev) + seq_len(n_blank))
    privatize_(nms_fill, nms)
  }
})

tidy_dots <- function(nms, nms_nondots) {
  dots <- nms[names(nms) %notin% nms_nondots]
  lapply(dots, eneval_tidy)
}

promise_tidy <- function(nms, nms_nondots, env) {
  nondots <- nms[names(nms) %in% nms_nondots]
  promises <- lapply(nondots, eneval_tidy)
  new_fn(promises, quote(environment()), env, eval_tidy = eval_tidy)
}

eneval_tidy <- function(nm) {
  call("eval_tidy", as.name(nm))
}

#' @rdname partial
#' @export
departial <- function(..f) {
  is.function(..f) %because% "Only functions can be de-partialized"
  departial_(..f)
}

departial_ <- local({
  get_bare <- getter("__bare__")
  function(f) get_bare(f) %||% f
})

#' @export
print.PartialFunction <- function(x, ...) {
  cat("<Partially Applied Function>\n\n")
  expr_print(expr_partial_closure(x))
  cat("\nRecover the inner function with 'departial()'.")
  invisible(x)
}

expr_partial_closure <- local({
  get_partial_closure <- getter("__partial__")

  function(x) {
    make_expr <- get_partial_closure(x)
    environment(make_expr) <-
      environment(x) %encloses% list(`__bare__` = call_with_fixed_args(x))
    make_expr()
  }
})

call_with_fixed_args <- function(x) {
  formals_fixed <- function(env) {
    fmls <- formals(x)
    fmls <- lapply(fmls, function(arg) expr_uq(subst_called_args(arg), env))
    as.pairlist(fmls)
  }
  body_fixed <- function(call) {
    call <- subst_called_args(call)
    args <- lapply(call[-1L], subst_formal_args)
    as.call(c(expr_partial(x), args))
  }
  subst_called_args <- local({
    nms_fix <- names_fixed(x)
    nms_fix <- nms_fix[names(nms_fix) %in% names(formals(departial_(x)))]
    exprs_fix <- lapply(nms_fix, function(nm) uq(as.name(nm)))

    function(expr) {
      do.call("substitute", list(expr, exprs_fix))
    }
  })

  function(...) {
    fmls_fixed <- formals_fixed(parent.frame())
    body_fixed <- body_fixed(sys.call())
    call_fixed <- call("function", fmls_fixed, call("{", body_fixed))
    expr_uq(call_fixed, parent.frame())
  }
}

subst_formal_args <- local({
  unquote <- list(eval_tidy = function(arg) uq(substitute(arg)))
  is_tidy_call <- check_head("eval_tidy")

  function(arg) {
    if (is.call(arg) && is_tidy_call(arg)) eval(arg, unquote) else arg
  }
})

expr_uq <- function(x, env) {
  eval(as.call(c(quote(rlang::expr), list(x))), env)
}

uq <- function(x) bquote(!!.(x))

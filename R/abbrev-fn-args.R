#' Enhance a function to interpret abbreviated function expressions
#'
#' `abbrev_fn_args()` is a functional operator that \dQuote{upgrades} a function
#' to one that can interpret abbreviated function expressions.
#'
#' @param f Function, or symbol or name of a function.
#' @param ... Name(s) of functional argument(s) of `f` (strings). Unsplicing of
#'   lists of strings is supported via `!!!` or `UQS()`.
#'
#' @return A function with the same call signature as `f`, but whose function
#'   arguments, as designated by `...`, may also be specified as abbreviated
#'   function expression of the form `.(...)`, cf. [as_fn()].
#'
#' @seealso [as_fn()]
#'
#' @examples
#' reduce <- abbrev_fn_args(Reduce, "f")
#' reduce(.(u, v ~ u + 1 / v), c(3, 7, 15, 1, 292), right = TRUE)
#'
#' @export
abbrev_fn_args <- function(f, ...) {
  f <- match.fun(f)
  fmls <- fn_fmls(f)
  interpret_anon_fns <- anon_fn_interpreter(names(fmls), ...)
  if (is.null(interpret_anon_fns))
    return(f)
  `formals<-`(
    function() {
      env_encl <- parent.env(environment())
      env_call <- parent.frame()
      call <- interpret_anon_fns(match.call(), env_call)
      eval_bare(mut_node_car(call, env_encl$f), env_call)
    },
    value = fmls
  )
}

anon_fn_interpreter <- function(nms, ...) {
  nms_fn <- chr(...)
  # detect emptiness here, rather than in caller, so that empty splice is empty
  if (is_empty(nms_fn))
    return(NULL)
  if (any(!nms_fn %in% nms))
    abort("Invalid argument name(s)")
  function(call, env) {
    call[nms_fn] <- lapply(call[nms_fn], interpret_fn, env = env)
    call
  }
}

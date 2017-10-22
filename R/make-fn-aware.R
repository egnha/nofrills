#' Make a function aware of abbreviated functional arguments
#'
#' `make_fn_aware()` is a functional operator that enhances a function by
#' enabling it to interpret abbreviated functional arguments.
#'
#' @param f Function, or symbol or name of a function.
#' @param ... Name(s) of functional argument(s) of `f` (strings) or `NULL`.
#'   Unsplicing of lists of strings is supported via `!!!` or `UQS()`.
#'
#' @return A function with the same call signature as `f`, but whose function
#'   arguments, as designated by `...`, may be specified using an abbreviated
#'   function expression of the form `.(...)`, cf. [as_fn()]. If `...` is empty
#'   or `NULL`, then `f` is simply returned.
#'
#' @seealso [as_fn()]
#'
#' @examples
#' reduce <- make_fn_aware(Reduce, "f")
#'
#' ## reduce() behaves just like Reduce()
#' Reduce(function(u, v) u + 1 / v, c(3, 7, 15, 1, 292), right = TRUE)
#' reduce(function(u, v) u + 1 / v, c(3, 7, 15, 1, 292), right = TRUE)
#'
#' ## reduce() can also interpret abbreviated function expressions
#' reduce(.(u, v ~ u + 1 / v), c(3, 7, 15, 1, 292), right = TRUE)
#'
#' @export
make_fn_aware <- function(f, ...) {
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
      call[[1]] <- env_encl$f
      eval(call, env_call)
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

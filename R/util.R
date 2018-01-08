#' Make a function
#'
#' `make_function()` extends [rlang::new_function()] to permit closures as
#' function bodies.
#'
#' @param args,body,env Same as for [rlang::new_function()], but `body` may also
#'   be a closure.
#' @return Function.
#'
#' @noRd
make_function <- function(args, body, env = caller_env()) {
  stopifnot(all(have_name(args)), is_env(env))
  if (is_closure(body)) {
    body <- call("function", formals(body), base::body(body))
  } else if (!is_expr(body)) {
    abort("Body must be an expression or closure.")
  }
  args <- as.pairlist(args)
  eval(call("function", args, body), env)
}

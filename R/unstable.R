# These functions override those in rlang that may be unstable

abort <- function(...) {
  stop(..., call. = FALSE)
}

warn <- function(...) {
  warning(..., call. = FALSE)
}

new_function <- function(args, body, env) {
  args <- as.pairlist(args)
  eval(call("function", args, body), env)
}

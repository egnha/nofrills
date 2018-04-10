# rlang::new_function() without checks
new_function_ <- function(args, body, env = parent.frame()) {
  eval(call("function", as.pairlist(args), body), env)
}

closure <- function(f) {
  if (typeof(f) == "closure")
    return(f)
  as_closure(f, parent.frame())
}

call_in_caller_env <- function(f, maybe_transform = NULL) {
  force(f)
  if (is.null(maybe_transform))
    return(
      function()
        eval(`[[<-`(sys.call(-1), 1, f), parent.frame(2))
    )
  function(...) {
    call <- maybe_transform(sys.call(-1), ...)
    eval(`[[<-`(call, 1, f), parent.frame(2))
  }
}

getter <- function(nm, maybe_transform = NULL) {
  force(nm)
  if (is.null(maybe_transform))
    return(function(env) .subset2(env, nm))
  function(env) .subset2(maybe_transform(env), nm)
}

setter <- function(nm) {
  force(nm)
  function(x, value) {
    assign(nm, value, envir = x)
    invisible(x)
  }
}

nondots <- function(xs)
  xs[xs != "..."]

has_dots <- function(x)
  match("...", x, nomatch = 0L) > 0L

eponymous <- function(nms) {
  names(nms) <- nms
  lapply(nms, as.name)
}

opposite <- function(f) {
  formals(f) <- rev(formals(f))
  f
}

box <- function(x)
  if (is.list(x)) x else list(x)

subst <- function(expr, vals)
  do.call("substitute", list(expr, vals))

`%notin%` <- function(these, those)
  match(these, those, nomatch = 0L) == 0L

`%are%` <- function(these, those)
  all(match(these, those, nomatch = 0L) > 0L)

`%because%` <- function(assertion, reason)
  if (!assertion) stop(reason, call. = FALSE)

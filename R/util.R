fun_getter <- function(nm) {
  get_fun <- getter(nm, mode = "function")
  function(x)
    get_fun(environment(x))
}

getter <- function(nm, mode) {
  force(nm)
  force(mode)
  function(env) {
    if (is.null(env))
      return(NULL)
    get0(nm, envir = env, mode = mode, inherits = FALSE)
  }
}

setter <- function(nm) {
  force(nm)
  function(x, value) {
    assign(nm, value, envir = x)
    invisible(x)
  }
}

names_nondots <- function(xs)
  nondots(names(xs))

nondots <- function(xs)
  xs[xs != "..."]

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

`%because%` <- function(assertion, reason) {
  if (!assertion)
    stop(interpolate_string(reason, parent.frame()), call. = FALSE)
}

#' Ad hoc string interpolation
#'
#' @param text String of the form `"...{*}..."`. Anything appearing in braces is
#'   `substitute()`'d, i.e., interpolated, in the environment `env`.
#' @param env Environment in which to interpolate `text`.
#'
#' @noRd
interpolate_string <- function(text, env) {
  matches <- gregexpr("\\{.*?\\}", text)
  if (none(matches))
    return(text)
  nms <- interpolated_names(text, matches, env)
  regmatches(text, matches) <- "%s"
  sprintf(text, nms)
}

none <- function(matches) matches[[1]] == -1

interpolated_names <- function(text, matches, env) {
  nms <- regmatches(text, matches)[[1]]
  nms <- gsub("[\\{\\}]", "", nms)
  vapply(nms, deparse_quote, character(1), env = env)
}

deparse_quote <- function(nm, env) {
  sym <- eval(bquote(substitute(.(as.name(nm)), env)))
  encodeString(deparse_str(sym), quote = "'")
}

deparse_str <- function(x) {
  d <- deparse(x)
  if (length(d) > 1)
    d <- paste(trimws(gsub("\\s+", " ", d), which = "left"), collapse = "")
  d
}

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

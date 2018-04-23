new_fn <- function(..args, ..body, ..env = NULL, ...) {
  if (!is.pairlist(..args))
    ..args <- as.pairlist(..args)
  if (missing(...))
    return(eval(call("function", ..args, ..body), ..env))
  eval(call("function", ..args, ..body), as.list(c(...)), ..env)
}

closure <- function(f) {
  if (typeof(f) == "closure")
    return(f)
  as_closure(f, parent.frame())
}

nondots <- function(xs)
  xs[xs != "..."]

has_dots <- function(x)
  match("...", x, nomatch = 0L) > 0L

eponymous <- function(nms)
  lapply(nms, as.name) %named% nms

opposite <- function(f) {
  formals(f) <- rev(formals(f))
  f
}

box <- function(x)
  if (is.list(x)) x else list(x)

`%notin%` <- function(these, those)
  match(these, those, nomatch = 0L) == 0L

`%are%` <- function(these, those)
  all(match(these, those, nomatch = 0L) > 0L)

`%because%` <- function(assertion, reason)
  if (!assertion) stop(reason, call. = FALSE)

`%subclass%` <- function(class, superclass) {
  wh_class <- which(superclass == class)
  if (is_empty(wh_class))
    return(c(class, superclass))
  if (isTRUE(wh_class == 1L))
    return(superclass)
  c(class, superclass[-wh_class])
}

getter_env <- function(nm) {
  force(nm)
  function(x) .subset2(environment(x), nm)
}

assign_getter <- function(nm, property = nm, env = parent.frame()) {
  force(property)
  getter <- function(x) attr(x, property, exact = TRUE)
  assign(nm, getter, envir = env)
}

assign_setter <- function(nm, property = nm, env = parent.frame()) {
  force(property)
  setter <- function(x, value) {
    attr(x, property) <- value
    invisible(x)
  }
  assign(paste0(nm, "<-"), setter, envir = env)
}

check_head <- function(nm) {
  sym <- as.name(nm)
  function(x) identical(x[[1]], sym)
}

`%binds%` <- function(env, bindings) {
  invisible(list2env(bindings, envir = env))
}

`%encloses%` <- function(parent, bindings) {
  list2env(bindings, parent = parent)
}

`%named%` <- `names<-`

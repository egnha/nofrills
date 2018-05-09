# Aliases
list_tidy <- list2
names_chr <- names2
`%named%` <- function(x, nm) `names<-`(x, nm)
fmt       <- function(text, ...) sprintf(text, ...)

new_fn <- function(..args, ..body, ..env = NULL, ...) {
  if (!is.pairlist(..args))
    ..args <- as.pairlist(..args)
  if (missing(...))
    return(eval(call("function", ..args, ..body), ..env))
  eval(call("function", ..args, ..body), list_tidy(...), ..env)
}

fml_args <- function(f) {
  formals(args(f) %||% as_closure(f))
}

closure <- function(f) {
  if (typeof(f) == "closure")
    return(f)
  as_closure(f, parent.frame())
}

eponymous <- function(nms) {
  lapply(nms, as.name) %named% nms
}

has_dots <- function(x) {
  match("...", x, nomatch = 0L) > 0L
}

`%notin%` <- function(these, those) {
  match(these, those, nomatch = 0L) == 0L
}

`%are%` <- function(these, those) {
  all(match(these, those, nomatch = 0L) > 0L)
}

is_onesided <- function(fml) {
  length(fml) == 2L
}

`%because%` <- function(assertion, reason) {
  if (!assertion) stop(reason, call. = FALSE)
  invisible(TRUE)
}

`%unless%` <- function(expr, failure) {
  tryCatch(expr, error = function(e) halt(failure, e$message))
}

halt <- function(msg, ...) {
  stop(fmt(msg, ...), call. = FALSE)
}

`%subclass%` <- function(class, superclass) {
  wh_class <- which(superclass == class)
  if (is_empty(wh_class))
    return(c(class, superclass))
  if (isTRUE(wh_class == 1L))
    return(superclass)
  c(class, superclass[-wh_class])
}

# nocov start (build-time only)
getter <- function(nm) {
  force(nm)
  function(x) .subset2(environment(x), nm)
}

assign_getter <- function(nm) {
  property <- mangle(nm)
  getter <- function(x) {
    attr(x, property, exact = TRUE)
  }
  assign(nm, getter, envir = parent.frame())
  invisible(getter)
}

assign_setter <- function(nm) {
  property <- mangle(nm)
  setter <- function(x, value) {
    attr(x, property) <- value
    invisible(x)
  }
  assign(paste0(nm, "<-"), setter, envir = parent.frame())
  invisible(setter)
}

mangle <- function(nm) {
  paste0(".__NOFRILLS_", toupper(nm), "__.")
}

check_head <- function(nm) {
  sym <- as.name(nm)
  function(x) identical(x[[1L]], sym)
}
# nocov end

`%binds%` <- function(env, bindings) {
  list2env(bindings, envir = env)
  invisible(env)
}

`%encloses%` <- function(parent, bindings) {
  list2env(bindings, parent = parent)
}

envir <- function(f) {
  environment(f) %||% baseenv()
}

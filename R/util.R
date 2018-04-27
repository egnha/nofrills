# Aliases
list_tidy <- list2
names_chr <- names2

new_fn <- function(..args, ..body, ..env = NULL, ...) {
  if (!is.pairlist(..args))
    ..args <- as.pairlist(..args)
  if (missing(...))
    return(eval(call("function", ..args, ..body), ..env))
  eval(call("function", ..args, ..body), list_tidy(...), ..env)
}

closure <- function(f) {
  if (typeof(f) == "closure")
    return(f)
  as_closure(f, parent.frame())
}

eponymous <- function(nms) {
  lapply(nms, as.name) %named% nms
}

# nocov start
opposite <- function(f) {
  formals(f) <- rev(formals(f))
  f
}
# nocov end

box <- function(x) {
  if (is.list(x)) x else list(x)
}

is_onesided <- function(fml) {
  length(fml) == 2L
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

`%because%` <- function(assertion, reason) {
  if (!assertion) stop(reason, call. = FALSE)
}

`%subclass%` <- function(class, superclass) {
  wh_class <- which(superclass == class)
  if (is_empty(wh_class))
    return(c(class, superclass))
  if (isTRUE(wh_class == 1L))
    return(superclass)
  c(class, superclass[-wh_class])
}

# nocov start
getter_env <- function(nm) {
  force(nm)
  function(x) .subset2(environment(x), nm)
}

assign_getter <- function(nm) {
  property <- obscure(nm)
  getter <- function(x) attr(x, property, exact = TRUE)
  assign(nm, getter, envir = parent.frame())
  invisible(getter)
}

assign_setter <- function(nm) {
  property <- obscure(nm)
  setter <- function(x, value) {
    attr(x, property) <- value
    invisible(x)
  }
  assign(paste0(nm, "<-"), setter, envir = parent.frame())
  invisible(setter)
}

obscure <- function(nm) {
  paste0(".__NOFRILLS_", toupper(nm), "__.")
}

check_caller <- function(nm) {
  sym <- as.name(nm)
  function(x) is.call(x) && identical(x[[1L]], sym)
}
# nocov end

`%binds%` <- function(env, bindings) {
  invisible(list2env(bindings, envir = env))
}

`%encloses%` <- function(parent, bindings) {
  list2env(bindings, parent = parent)
}

`%named%` <- `names<-`

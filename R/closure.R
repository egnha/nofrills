#' Express a function as a closure
#'
#' Unlike [rlang::as_closure()], the formals of `closure()` agree with those
#' of [args()].
#'
#' @param f Function.
#'
#' @return If `f` is a closure, it is simply returned. Otherwise, `f` is a
#'   primitive function, and `closure()` wraps it in a closure whose environment
#'   is `.BaseNamespaceEnv` and whose formals are those of `args()`, unless this
#'   is `NULL`, in which case an error is raised.
#'
#' @noRd
closure <- function(f) {
  if (typeof(f) == "closure")
    return(f)
  nm <- primitive_name(f)
  if (!is.null(prim <- primitive_op[[nm]])) {
    if (inherits(prim, "NonclosurePrimitive")) stop(prim)
    return(prim)
  }
  fmls <- formals(args(nm))
  # Skip checks in new_fn()
  eval(call("function", fmls, invoke_primitive(nm, fmls)), .BaseNamespaceEnv)
}

# rlang::prim_name() without validation
primitive_name <- function(prim) {
  nm <- format(prim)
  sub("\"\\)$", "", sub("^.Primitive\\(\"", "", nm))
}

invoke_primitive <- function(nm, fmls) {
  nms <- names(fmls)
  args <- `names<-`(lapply(nms, as.name), character(length(nms)))
  if (!is.na(pos <- match("...", nms)))
    names(args)[-seq_len(pos)] <- nms[-seq_len(pos)]
  as.call(c(call(".Primitive", nm), args))
}

primitive_op <- list(
  `:`    = function(a, b) a:b,
  `&&`   = function(x, y) x && y,
  `||`   = function(x, y) x || y,
  `[`    = function(x, ...) x[...],
  `[[`   = function(x, ...) x[[...]],
  `[[<-` = function(x, ..., value) `[[<-`(x, ..., value = value),
  `[<-`  = function(x, ..., value) `[<-`(x, ..., value = value)
)
primitive_op <- lapply(primitive_op, `environment<-`, value = .BaseNamespaceEnv)
primitive_op <- list2env(primitive_op, parent = emptyenv())

# No good way to define these primitives as closures; raise error if you try.
for (op in c(
  "(", "{", "$", "$<-", "@", "@<-", "<-", "<<-", "=", "~",
  "if", "for", "while", "break", "next", "repeat", "function", "return"
)) {
  err <- sprintf("Expressing `%s` as a closure is not supported", op)
  primitive_op[[op]] <- structure(
    list(message = err, call = NULL),
    class = c("NonclosurePrimitive", "error", "condition")
  )
}

# Ensure that the remaining primitives are those with formals
stopifnot({
  objs <- objects("package:base", all.names = TRUE)
  prim <- sapply(objs, function(x) is.primitive(get(x)))
  no_formals <- !objs[prim] %in% union(names(.ArgsEnv), names(.GenericArgsEnv))
  setequal(names(primitive_op), objs[prim][no_formals])
})

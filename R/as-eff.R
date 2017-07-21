#' Minimal notation for arguments that are functions
#'
#' `as_eff()` is for functions that take functional arguments. Use `as_eff()`
#' _inside_ a function to enable it to accept a minimal anonymous-function
#' notation for arguments that are functions. This notation is that of [eff()],
#' but with \sQuote{`eff`} replaced by \sQuote{`.`}—it is scarcely possible for
#' a general anonymous-function notation to use fewer tokens than that.
#'
#' @param .f A function or an abbreviated anonymous-function expression of the
#'   form `.(...)`, where `...` is a [function declaration][eff()] (i.e., `.`,
#'   in this context, is an alias of [eff()]). As for [eff()],
#'   [quasiquotation][rlang::quasiquotation] is supported.
#'
#' @return If `.f` is a function, it is simply returned, otherwise the function
#'   of the [function declaration][eff()] is returned.
#'
#' @details `as_eff()` cannot follow promise expressions across function calls.
#'   It is only intended to work in the immediate context in which a function
#'   declaration is to be interpreted (see _Examples_).
#'
#' @seealso [eff()]
#'
#' @examples
#' call_fn <- function(.f, x) {
#'   f <- as_eff(.f)
#'   f(x)
#' }
#' call_fn(log, 1)
#' call_fn(.(. ~ sin(.) ^ 2), 1)
#' # simplified function expressions support quasiquotation
#' f <- sin
#' call_fn(.(. ~ UQ(f)(.) ^ 2), 1)
#'
#' ## wrap Map() to accept abbreviated anonymous function expressions
#' Map_ <- function (f, ...) {
#'   f <- as_eff(f)
#'   mapply(FUN = f, ..., SIMPLIFY = FALSE)
#' }
#' # you can call Map_() just like Map()
#' Map_(function(x, y, z) paste(x, y, paste("and", z), sep = ", "), 1:3, 4:6, 7:9)
#' # or use a simplified function expression
#' Map_(.(x, y, z ~ paste(x, y, paste("and", z), sep = ", ")), 1:3, 4:6, 7:9)
#'
#' ## abbreviated anonymous functions are interpreted in the calling environment
#' # so this works, as expected
#' foo <- function(a) as_eff(a)
#' foo(.(x ~ x + 1))
#' # but as_eff() can't interpret abbreviated anonymous functions across calls
#' foo <- function(a) bar(a)
#' bar <- function(b) as_eff(b)
#' \dontrun{
#' foo(.(x ~ x + 1))}
#'
#' @export
as_eff <- function(.f) {
  x <- eval_bare(substitute(rlang::enexpr(.f)), parent.frame())
  if (is_anon_fn_expr(x))
    eval_bare(mut_node_car(x, eff), parent.frame(2))
  else
    match.fun(.f)
}

is_anon_fn_expr <- function(x) {
  is.call(x) && identical(x[[1]], as.name("."))
}
#' Abbreviated functional arguments
#'
#' `as_fn()` is for functions that take functional arguments. Use `as_fn()`
#' _inside_ a function to enable it to comprehend a minimal anonymous-function
#' notation for arguments that are functions. This notation is that of [fn()],
#' but with \sQuote{`fn`} replaced by \sQuote{`.`} (dot).
#'
#' @param .f A function or an abbreviated anonymous-function expression of the
#'   form `.(...)`, where `...` is a [function declaration][fn()] (i.e., `.`
#'   (dot) in this context is an alias of [fn()]).
#'   [Quasiquotation][rlang::quasiquotation] is supported.
#'
#' @return If `.f` is a function, it is simply returned, otherwise the function
#'   determined by the [function declaration][fn()] is returned.
#'
#' @details `as_fn()` cannot follow promise expressions across function calls.
#'   It is only intended to work in the immediate context in which a function
#'   declaration is to be interpreted (see _Examples_).
#'
#' @seealso [fn()], [make_fn_aware()]
#'
#' @examples
#' call_fn <- function(.f, x) {
#'   f <- as_fn(.f)
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
#'   f <- as_fn(f)
#'   mapply(FUN = f, ..., SIMPLIFY = FALSE)
#' }
#' # you can call Map_() just like Map()
#' Map_(function(x, y, z) paste(x, y, paste("and", z), sep = ", "), 1:3, 4:6, 7:9)
#' # or use a simplified function expression
#' Map_(.(x, y, z ~ paste(x, y, paste("and", z), sep = ", ")), 1:3, 4:6, 7:9)
#'
#' ## abbreviated anonymous functions are interpreted in the calling environment
#' # so this works, as expected
#' foo <- function(a) as_fn(a)
#' foo(.(x ~ x + 1))
#' # but as_fn() can't interpret abbreviated anonymous functions across calls
#' foo <- function(a) bar(a)
#' bar <- function(b) as_fn(b)
#' \dontrun{
#' foo(.(x ~ x + 1))}
#'
#' @export
as_fn <- function(.f) {
  x <- enexpr(.f)
  x <- eval(substitute(substitute(x)), parent.frame())
  interpret_fn(x, match.fun(.f), parent.frame(2))
}

interpret_fn <- function(x, f = x, env) {
  if (is_anon_fn_expr(x)) {
    x[[1]] <- fn
    eval(x, env)
  } else
    f
}

is_anon_fn_expr <- local({
  sym_dot <- as.name(".")
  function(x)
    is.call(x) && identical(x[[1]], sym_dot)
})

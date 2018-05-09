context("Printing")

test_that("partially applied function shows function with arguments fixed", {
  replace <- TRUE
  draw <- function(x, size = length(x), replace = FALSE, prob = NULL) {
    sample(x, size, replace, prob)
  }
  draw_letters <- partial(draw, letters, replace = !!replace)
  out <- c(
    "<Partially Applied Function>",
    "",
    "function(size = length(^letters), prob = NULL) {",
    "  draw(x = ^letters, size = size, replace = ^TRUE, prob = prob)",
    "}",
    "",
    "Recover the inner function with 'departial()'."
  )
  expect_identical(capture.output(print(draw_letters)), out)
})

test_that("tidy function shows underlying function", {
  f <- tidy(identity)
  out_identity <- capture.output(print(identity))
  out <- c(
    "<Tidy Function>",
    "",
    out_identity,
    "",
    "Recover the function shown with 'untidy()'."
  )
  expect_identical(capture.output(print(f)), out)
})

test_that("composition of functions shows composite functions", {
  f <- compose(abs, inc = partial(`+`, 1), log)
  out <- c(
    "<Function Composition>",
    "From the inner to outer function:",
    "",
    " 1. function (x)  .Primitive(\"abs\")",
    "",
    " 2. $inc",
    "    <Partially Applied Function>",
    "    function(.y) {",
    "      (^1) + .y",
    "    }",
    "",
    " 3. function (x, base = exp(1))  .Primitive(\"log\")",
    "",
    "Recover the list of functions with 'as.list()'."
  )
  expect_identical(capture.output(print(f)), out)
})

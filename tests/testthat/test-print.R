context("Printing")

test_that("partially applied function shows function with arguments fixed", {
  replace <- TRUE
  f <- partial(sample, size = expr, replace = !!replace)
  out <- c(
    "<Partially Applied Function>",
    "",
    "function(x, prob = NULL) {",
    "  sample(x = x, size = ^expr, replace = ^TRUE, prob = prob)",
    "}",
    "",
    "Recover the inner function with 'departial()'."
  )
  expect_identical(capture.output(print(f)), out)
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
  f <- compose(log, inc = partial(`+`, 1), abs)
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

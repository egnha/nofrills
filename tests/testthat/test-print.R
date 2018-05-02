context("Printing")

test_that("partially applied function shows function with arguments fixed", {
  f <- partial(sample, size = 3, replace = TRUE)
  out <- c(
    "<Partially Applied Function>",
    "",
    "function(x, prob = NULL) {",
    "  sample(x = x, size = ^3, replace = ^TRUE, prob = prob)",
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
  f <- compose(log, partial(`+`, 1), abs)
  out <- c(
    "<Function Composition (in calling order)>",
    "",
    " 1: function (x)  .Primitive(\"abs\")",
    "",
    " 2: <Partially Applied Function>",
    "    function(.y) {",
    "      (^1) + .y",
    "    }",
    "",
    " 3: function (x, base = exp(1))  .Primitive(\"log\")",
    "",
    "Recover the list of functions with 'decompose()'."
  )
  expect_identical(capture.output(print(f)), out)
})

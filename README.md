
<!-- README.md is generated from README.Rmd. Please edit that file -->

> Unless you need `curry()` or `curry_fn()`, you should use the more
> versatile [gestalt](https://github.com/egnha/gestalt) package, which
> includes `fn()`.

[![Travis-CI Build
Status](https://travis-ci.org/egnha/nofrills.svg?branch=master)](https://travis-ci.org/egnha/nofrills)
[![codecov](https://codecov.io/gh/egnha/nofrills/branch/master/graph/badge.svg)](https://app.codecov.io/gh/egnha/nofrills)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/nofrills)](https://cran.r-project.org/package=nofrills)

# nofrills <img src="inst/logo.png" align="right" />

*Low-Cost Anonymous Functions*

## Overview

*nofrills* is a lightweight R package that provides `fn()`, a more
powerful variation of `function()` that:

-   **costs less** — enables tidyverse quasiquotation so you don’t pay
    the price of [functional
    impurity](#pure-functions-via-quasiquotation)

-   has the **same great taste** — supports a superset of `function()`’s
    syntax and capabilities

-   is **less filling** —

    ``` r
    fn(x, y = 1 ~ x + y)
    ```

    is equivalent to

    ``` r
    function(x, y = 1) x + y
    ```

## Installation

``` r
install.packages("nofrills")
```

Alternatively, install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("egnha/nofrills")
```

## Usage

### Same syntax as `function()` but shorter

``` r
fn(x ~ x + 1)
#> function (x) 
#> x + 1

fn(x, y ~ x + y)
#> function (x, y) 
#> x + y

fn(x, y = 2 ~ x + y)
#> function (x, y = 2) 
#> x + y

fn(x, y = 1, ... ~ log(x + y, ...))
#> function (x, y = 1, ...) 
#> log(x + y, ...)

# the only exception, cf. alist()
fn(x, ... = , y ~ log(x + y, ...))
#> function (x, ..., y) 
#> log(x + y, ...)

fn(~ NA)
#> function () 
#> NA
```

### Supports quasiquotation

#### Unquote values

``` r
z <- 0

fn(x, y = !!z ~ x + y)
#> function (x, y = 0) 
#> x + y

fn(x ~ x > !!z)
#> function (x) 
#> x > 0
```

#### Unquote argument names

``` r
arg <- "y"

fn(x, !!arg := 0 ~ x + !!as.name(arg))
#> function (x, y = 0) 
#> x + y
```

#### Splice in argument lists

``` r
args <- alist(x, y = 0)

fn(!!!args, ~ x + y)  # note the one-sided formula
#> function (x, y = 0) 
#> x + y
```

#### Literally unquote with `QUQ()`, `QUQS()`

``` r
library(dplyr)

summariser <- quote(mean)

my_summarise <- fn(df, ... ~ {
  group_by <- quos(...)
  df %>%
    group_by(QUQS(group_by)) %>%
    summarise(a = (!!summariser)(a))
})

my_summarise
#> function (df, ...) 
#> {
#>     group_by <- quos(...)
#>     df %>% group_by(`!!!`(group_by)) %>% summarise(a = mean(a))
#> }
```

(Source: [*Programming with
dplyr*](https://dplyr.tidyverse.org/articles/programming.html))

### [Curry](https://en.wikipedia.org/wiki/Currying) functions

#### Declare a curried function with `curry_fn()`

The syntax is the same as `fn()`. Using the literal unquoting operators
`QUQ()`, `QUQS()`, you can “delay” unquoting to embed argument values in
the innermost function:

``` r
compare_to <- curry_fn(target, x ~ identical(x, QUQ(target)))
is_this <- compare_to("this")

# The embedded value "this" renders the source comprehensible
is_this
#> function (x) 
#> identical(x, "this")
#> <environment: 0x7fdc55943678>
```

#### Curry a function with `curry()`

``` r
curry(function(x, y, z = 0) x + y + z)
#> function (x) 
#> function(y) function(z = 0) x + y + z

double <- curry(`*`)(2)
double(3)
#> [1] 6
```

## Pure functions via quasiquotation

Functions in R are generally
[impure](https://en.wikipedia.org/wiki/Pure_function), i.e., the return
value of a function will *not* in general be determined by the value of
its inputs alone. This is because a function may depend on mutable
objects in its [lexical
scope](https://adv-r.hadley.nz/functions.html#lexical-scoping). Normally
this isn’t an issue. But if you are working interactively and sourcing
files into the global environment, say, or using a notebook interface
(like Jupyter or R Notebook), it can be tricky to ensure that you
haven’t unwittingly mutated an object that an earlier function depends
upon.

-   Consider the following function:

    ``` r
    a <- 1
    foo <- function(x) x + a
    ```

    What is the value of `foo(1)`? It is not necessarily `2` because the
    value of `a` may have changed between the *creation* of `foo()` and
    the *calling* of `foo(1)`:

    ``` r
    foo(1)
    #> [1] 2

    a <- 0

    foo(1)
    #> [1] 1
    ```

    In other words, `foo()` is impure because the value of `foo(x)`
    depends not only on the value of `x` but also on the *externally
    mutable* value of `a`.

`fn()` enables you to write **pure(r)** functions by using
quasiquotation to eliminate such indeterminacy.

-   With `fn()`, you can unquote `a` to capture its value at the point
    of creation:

    ``` r
    a <- 1
    foo <- fn(x ~ x + !!a)
    ```

    Now `foo()` is a pure function, unaffected by changes in its lexical
    scope:

    ``` r
    foo(1)
    #> [1] 2

    a <- 0

    foo(1)
    #> [1] 2
    ```

## Alternatives to nofrills

Alternative anonymous-function constructors (which don’t support
quasiquotation) include:

-   [`pryr::f()`](https://github.com/hadley/pryr)
-   [`lambda::f()`](https://github.com/jimhester/lambda)
-   [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html)

## Acknowledgement

The [rlang](https://github.com/r-lib/rlang) package by [Lionel
Henry](https://github.com/lionel-) and [Hadley
Wickham](https://github.com/hadley) makes nofrills possible. Crucially,
rlang provides the engine for quasiquotation and expression capture.

## License

MIT Copyright © 2017–22 [Eugene Ha](https://github.com/egnha)

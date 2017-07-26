
<!-- README.md is generated from README.Rmd. Please edit that file -->
nofrills <img src="logo.png" align="right" />
=============================================

*Low-Cost Anonymous Functions*

[![Travis-CI Build Status](https://travis-ci.org/egnha/nofrills.svg?branch=master)](https://travis-ci.org/egnha/nofrills) [![codecov](https://codecov.io/gh/egnha/nofrills/branch/master/graph/badge.svg)](https://codecov.io/gh/egnha/nofrills) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/nofrills)](https://cran.r-project.org/package=nofrills) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Overview
--------

*nofrills* is a lightweight R package that provides `fn()`, a more powerful variation of `function()` that:

-   **costs less** — it enables Tidyverse [**quasiquotation**](http://rlang.tidyverse.org/reference/quasiquotation.html) for extra [safety](#protect-functions-against-scope-changes), when you need it
-   has the **same great taste** — supports a superset of `function()`’s syntax and capabilities
-   is **less filling** —

    ``` r
    fn(x, y = 1 ~ x + y)

    ..(x, y = 1 ~ x + y)
    ```

    are both equivalent to

    ``` r
    function(x, y = 1) x + y
    ```

Installation
------------

``` r
install.packages("nofrills")
```

Alternatively, install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("egnha/nofrills")
```

Usage
-----

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

#### Unquoting values

``` r
z <- 0

fn(x, y = !! z ~ x + y)
#> function (x, y = 0) 
#> x + y

fn(x ~ x > !! z)
#> function (x) 
#> x > 0
```

#### Unquoting argument names

``` r
arg <- "y"

fn(x, !! arg := 0 ~ x + !! as.name(arg))
#> function (x, y = 0) 
#> x + y
```

#### Splicing in argument lists

``` r
args <- alist(x, y = 0)

fn(!!! args, ~ x + y)  # note the one-sided formula
#> function (x, y = 0) 
#> x + y
```

### Protect functions against scope changes

By enabling [quasiquotation](http://rlang.tidyverse.org/reference/quasiquotation.html), `fn()` allows you to “burn in” values, which guards your function from being affected by unexpected scope changes.

-   **Example** — Both `f()` and `f_solid()` return the same value of x

    ``` r
    x <- "x"

    f <- function() x
    f_solid <- fn(~ !! x)

    f()
    #> [1] "x"

    f_solid()
    #> [1] "x"
    ```

    But if the binding `x` is (unwittingly) changed, `f()` changes, while `f_solid()` remains unaffected.

    ``` r
    x <- sin

    f()
    #> function (x)  .Primitive("sin")

    f_solid()
    #> [1] "x"
    ```

### 😃 functions

Pop quiz!—These smileys produce functions

``` r
..(~8^D)
..(8~D)
```

but which one is actually callable?

Alternatives to *nofrills*
--------------------------

Here are some alternative anonymous-function constructors (which don’t support quasiquotation), ordered by increasing concision and decreasing flexibility:

-   [`pryr::f()`](https://github.com/hadley/pryr)
-   [`lambda::f()`](https://github.com/jimhester/lambda)
-   [`rlang::as_function()`](http://rlang.tidyverse.org/reference/as_function.html)

Acknowledgement
---------------

The [rlang](https://github.com/tidyverse/rlang) package by [Lionel Henry](https://github.com/lionel-) and [Hadley Wickham](https://github.com/hadley) makes *nofrills* possible. Crucially, rlang provides the engine for quasiquotation and expression capture.

License
-------

MIT Copyright © 2017 [Eugene Ha](https://github.com/egnha)

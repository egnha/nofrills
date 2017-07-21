
<!-- README.md is generated from README.Rmd. Please edit that file -->
nofrills
========

*Everyday low-cost anonymous functions*

[![Travis-CI Build Status](https://travis-ci.org/egnha/nofrills.svg?branch=master)](https://travis-ci.org/egnha/nofrills) [![codecov](https://codecov.io/gh/egnha/nofrills/branch/master/graph/badge.svg)](https://codecov.io/gh/egnha/nofrills)

Overview
--------

*nofrills* is a tiny R package that provides a function `eff()`, which enables you to create (anonymous) functions, of *arbitrary* call signature. It’s a lower cost, drop-in replacement for the usual `function(<arguments>) <body>` invocation, in that:

-   it is **shorter**:

    ``` r
    eff(x, y = 1 ~ x + y)

    ..(x, y = 1 ~ x + y)
    ```

    are both equivalent to

    ``` r
    function(x, y = 1) x + y
    ```

-   it is **safer**: by enabling [quasiquotation](http://rlang.tidyverse.org/reference/quasiquotation.html), `eff()` allows you to “burn in” values, which guards your function from being affected by unexpected scope change

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("egnha/nofrills")
```

Usage
-----

For details and more examples, see the package documentation (`?eff`).

### Same syntax as `function()`, just shorter

``` r
eff(x ~ x + 1)
#> function (x) 
#> x + 1

eff(x, y ~ x + y)
#> function (x, y) 
#> x + y

eff(x, y = 2 ~ x + y)
#> function (x, y = 2) 
#> x + y

eff(x, y = 1, ... ~ log(x + y, ...))
#> function (x, y = 1, ...) 
#> log(x + y, ...)

# to specify `...` in the middle, write `... = `.
eff(x, ... = , y ~ log(x + y, ...))
#> function (x, ..., y) 
#> log(x + y, ...)

eff(~ NA)
#> function () 
#> NA
```

### Supports quasiquotation

#### Unquoting values

``` r
z <- 0
eff(x, y = !! z ~ x + y)
#> function (x, y = 0) 
#> x + y
eff(x ~ x > !! z)
#> function (x) 
#> x > 0
```

#### Unquoting argument names

``` r
arg <- "y"
eff(x, !! arg := 0 ~ x + !! as.name(arg))
#> function (x, y = 0) 
#> x + y
```

#### Splicing in argument lists

``` r
args <- alist(x, y = 0)
eff(!!! args, ~ x + y)
#> function (x, y = 0) 
#> x + y
```

(Note that the function body here is a one-sided formula.)

#### Protect functions against scope changes

``` r
x <- "x"
f <- function() x
f_solid <- eff(~ !! x)
```

Both return the same value of x

``` r
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

### Smiley functions

A riddle: Both of these smileys produce functions.

``` r
..(~8^D)
..(8~D)
```

But which one is actually callable?

Alternatives
------------

The following functions also create (anonymous) functions. They are able to be more concise than `eff()` and `..()`, because they are specialized rather than general.

-   [`rlang::as_function()`](http://rlang.tidyverse.org/reference/as_function.html) allows you to create anonymous functions of up to two arguments.

-   The [lambda](https://github.com/jimhester/lambda) package uses a `bquote()`-like notation for function declarations, which are very compact, because you dont’t configure the call signature. Quasiquotation is not supported.

License
-------

MIT Copyright © 2017 [Eugene Ha](https://github.com/egnha)

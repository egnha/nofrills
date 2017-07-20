
<!-- README.md is generated from README.Rmd. Please edit that file -->
nofrills
========

[![Travis-CI Build Status](https://travis-ci.org/egnha/nofrills.svg?branch=master)](https://travis-ci.org/egnha/nofrills) [![codecov](https://codecov.io/gh/egnha/nofrills/branch/master/graph/badge.svg)](https://codecov.io/gh/egnha/nofrills)

Low-cost anonymous functions

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("egnha/nofrills")
```

Usage
-----

``` r
library(nofrills)

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

## to specify '...' in the middle, write '... = '
eff(x, ... = , y ~ log(x + y, ...))
#> function (x, ..., y) 
#> log(x + y, ...)

## use one-sided formula for constant functions or commands
eff(~ NA)
#> function () 
#> NA
eff(~ message("!"))
#> function () 
#> message("!")

## unquoting is supported (using `!!` or UQ() from rlang)
zero <- 0
eff(x = UQ(zero) ~ x > !! zero)
#> function (x = 0) 
#> x > 0

## formals and function bodies can also be spliced in
f <- function(x, y) x + y
g <- function(y, x, ...) x - y
frankenstein <- eff(!!! formals(f), ~ !! body(g))
frankenstein
#> function (x, y) 
#> x - y

## unquoting protects against changes in a function’s scope
x <- "x"
f <- function() x
f_solid <- eff(~ !! x)
# both return the same value of x
f()
#> [1] "x"
f_solid()
#> [1] "x"
# but if the binding `x` is (unwittingly) changed, f() changes ...
x <- sin
f()
#> function (x)  .Primitive("sin")
# ... while f_solid() remains unaffected
f_solid()
#> [1] "x"
```

License
-------

MIT Copyright © 2017 [Eugene Ha](https://github.com/egnha)

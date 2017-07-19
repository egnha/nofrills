
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

f <- eff(x ~ x + 1)
f(1)
#> [1] 2

f <- eff(x, y ~ x + y)
f(1, 2)
#> [1] 3

f <- eff(x, y = 2 ~ x + y)
f(1)
#> [1] 3

f <- eff(x, y = 1, ... ~ log(x + y, ...))
f(1, 1, base = 2)
#> [1] 1

# to specify '...' in the middle of the call signature, write '... = '
f <- eff(x, ... = , y ~ log(x + y, ...))
f(1, base = 2, y = 1)
#> [1] 1

# use one-sided formula for constant functions or commands
eff(~ NULL)
#> function () 
#> NULL
eff(~ message("!"))
#> function () 
#> message("!")

# unquoting (via `!!` or UQ()) is supported
zero <- 0
eff(x = UQ(zero) ~ x > !! zero)
#> function (x = 0) 
#> x > 0

# formals and function bodies can also be spliced in
add <- function(x, y) x + y
sub <- function(y, x) x - y
frankenstein <- eff(!!! formals(add), ~ !! body(sub))
frankenstein
#> function (x, y) 
#> x - y
```

License
-------

MIT Copyright © 2017 [Eugene Ha](https://github.com/egnha)

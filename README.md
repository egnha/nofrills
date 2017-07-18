
<!-- README.md is generated from README.Rmd. Please edit that file -->
nofrills
========

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

# unquoting of names is supported
zero <- 0
is_positive <- eff(x ~ x > !! zero)
is_positive(1)
#> [1] TRUE
is_positive(-1)
#> [1] FALSE
```

License
-------

MIT Copyright © 2017 [Eugene Ha](https://github.com/egnha)

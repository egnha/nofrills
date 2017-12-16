In response to a notification from CRAN admin (KH), this minor update repairs
spurious test failures introduced by the 2.0.0 update of testthat.

## Test environments

* OS X 10.12.6 (local): R 3.4.3
* Ubuntu 14.04.5 LTS (on Travis CI): R 3.3.3, 3.4.2, devel (2017-12-15 r73917)
* Windows (on win-builder): R 3.3.3

## `R CMD check` results

On Mac OS and Linux (Ubuntu), there were no ERRORs, WARNINGSs or NOTEs.

On Windows, there were no ERRORs or WARNINGSs, and one NOTE:

> * checking CRAN incoming feasibility ... NOTE
> Maintainer: 'Eugene Ha <eha@posteo.de>'
> 
> License components with restrictions and base license permitting such:
>   MIT + file LICENSE
> File 'LICENSE':
>   YEAR: 2017
>   COPYRIGHT HOLDER: Eugene Ha
> 
> Possibly mis-spelled words in DESCRIPTION:
>   quasiquotation (7:52)
>   tidyverse (7:36)

The words "tidyverse" and "quasiquotation" are correctly spelt.

## Downstream dependencies

None.

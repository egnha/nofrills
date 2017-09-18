## Test environments

* OS X 10.12.6 (local): R 3.4.1
* Ubuntu 14.04.5 LTS (on Travis CI): R 3.3.3, 3.4.1
* Windows (on win-builder): R 3.3.3, 3.4.1, devel (2017-09-12 r73242)

## R CMD check results

There were no ERRORs or WARNINGSs.

There was one NOTE:

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

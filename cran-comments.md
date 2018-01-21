## Test environments

* macOS 10.12.6: R 3.4.3
* Ubuntu 14.04.5 LTS (Travis CI): R 3.3.3, 3.4.2, devel (2018-01-20 r74146)
* Windows (win-builder): R 3.3.3, 3.4.3, devel (2018-01-19 r74138)

## `R CMD check` results

On macOS, Linux, and Windows (R-devel) there were no ERRORs, WARNINGSs or NOTEs.

On Windows R 3.3.3, 3.4.3, there were no ERRORs or WARNINGSs, and one NOTE:

> * checking CRAN incoming feasibility ... NOTE
> Maintainer: 'Eugene Ha <eha@posteo.de>'
> 
> License components with restrictions and base license permitting such:
>   MIT + file LICENSE
> File 'LICENSE':
>   YEAR: 2017, 2018
>   COPYRIGHT HOLDER: Eugene Ha
> 
> Possibly mis-spelled words in DESCRIPTION:
>   quasiquotation (7:52)
>   tidyverse (7:36)

The words "tidyverse" and "quasiquotation" are correctly spelt.

## Downstream dependencies

None.

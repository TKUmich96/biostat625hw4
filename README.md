# biostat625hw4
In this repository, a package called `lm2Wsmry`, which is a package that tries to reproduce perfermance of `lm()` and `summary(lm())` in base.

<!-- badges: start -->
[![R-CMD-check](https://github.com/TKUmich96/biostat625hw4/workflows/R-CMD-check/badge.svg)](https://github.com/TKUmich96/biostat625hw4/actions)

[![codecov](https://codecov.io/gh/TKUmich96/biostat625hw4/branch/main/graph/badge.svg?token=26XY3PGHNB)](https://codecov.io/gh/TKUmich96/biostat625hw4)
<!-- badges: end -->

## Installation
To ultilize the package `lm2Wsmry`, download from GitHub and load in your environment by `devtools`
```r
devtools::install_github("TKUmich96/biostat625hw4", build_vignettes = T)
library(lm2Wsmry)
```

## Overview

The package contains two functions that used to fit a linear regression model, and return summary of important statistics of that regression. It could somehow reproduce the performance of `lm()` and `summary(lm())`
1. `lm2()`: a function used to fit the linear regresion
2. `summary_lm2()`: a function used to display the important statistics of that regression model from `lm2()`

Please vignettes in tutorial.Rmd for more details

#### Note On Rcpp usage
Rcpp code has been de-actived in the src file. When I test it locally, it worked, but cannot pass continuous integration. 
If you want to check how it helps to speed up my calculation, for free to comment it back.

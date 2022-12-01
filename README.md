
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/lan-k/CXOwt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lan-k/CXOwt/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/lan-k/CXOwt/branch/main/graph/badge.svg?token=TYMUR3Z03T)](https://codecov.io/gh/lan-k/CXOwt)
<!-- badges: end -->

# CXOwt

This R package was is code to implement weighted conditional logistic
regression described
[here](https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-021-01408-5).

## Installation

`CXOwt` is still under development. You can install the latest version
from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("lan-k/CXOwt")
```

## Using the package

`CXOwt` contains functions for both case-crossover and case-time-control
designs. There are 2 example dataframes, `cases` and `casetimecontrols`.

`CXO_wt_boot()` is the version for case-crossover studies,
`CXO_tc_wt_boot()` for case-time-control studies. Both return the
weighted Odds Ratio estimate and bootstrapped 95% confidence intervals.

``` r
library(CXOwt)

#case-crossover
data(cases)
cfit.b <- CXO_wt_boot(data=cases, exposure = ex, event = Event, Id=Id, B=500) 
summary(cfit.b)

#case-time-control
data(casetimecontrols)
ctcfit.b <- CXO_tc_wt_boot(data=casetimecontrols, exposure = ex, event = Event, Id=Id, B = 500) 
summary(ctcfit.b)
```

Alternatively, you can return the weighted conditional logistic
regression objects without bootstrapping:

``` r
#case-crossover
cfit <- CXO_wt(cases, exposure = ex, event = Event, Id=Id)  
summary(cfit)

#case-time-control
ctcfit <- CXO_tc_wt(casetimecontrols, exposure = ex, event = Event, Id=Id)   
summary(ctcfit)
```

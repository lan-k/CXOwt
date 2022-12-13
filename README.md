
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

## Data

Data should be structured with one row per period per person. The data
should be ordered so that for each person, the earliest period appears
first in the data, the case period appears last.

The following 3 variables are required: Patient ID Binary exposure
indicator (0 for unexposed or 1 for exposed) Binary indicator for the
outcome

- 0 in control periods
- 1 in case period for cases
- 0 in case period for time controls

The package comes with two example dataframes

- “cases” with cases only
- “casetimecontrols” with cases and time controls

The exposure variable is “ex” and the outcome variable is “Event”.

``` r
library(CXOwt)

#case-crossover
data(cases)

head(cases)
#> # A tibble: 6 × 6
#>      Id    ex Event   day   age   sex
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     2     1     0     1    48     0
#> 2     2     1     0     2    48     0
#> 3     2     1     0     3    48     0
#> 4     2     0     0     4    48     0
#> 5     2     0     0     5    48     0
#> 6     2     0     0     6    48     0

tail(cases) # last few rows of data for a case
#> # A tibble: 6 × 6
#>      Id    ex Event   day   age   sex
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1   499     1     0    85    33     1
#> 2   499     1     0    86    33     1
#> 3   499     1     0    87    33     1
#> 4   499     1     0    88    33     1
#> 5   499     1     0    89    33     1
#> 6   499     1     1    90    33     1

#case-time-controls
data(casetimecontrols)

tail(casetimecontrols[casetimecontrols$Id == 2,]) # last few rows of data for a time control
#> # A tibble: 6 × 6
#>      Id    ex Event   day   age   sex
#>   <int> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     2     0     0    85    44     0
#> 2     2     0     0    86    44     0
#> 3     2     0     0    87    44     0
#> 4     2     0     0    88    44     0
#> 5     2     0     0    89    44     0
#> 6     2     0     0    90    44     0
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
regression objects without bootstrapping. However, the standard errors
and 95% confidence intervals may not be accurate. The output will be a
‘clogit’ object.

``` r
#case-crossover
cfit <- CXO_wt(cases, exposure = ex, event = Event, Id=Id)  
exp(cbind(coef(cfit), confint(cfit)))
#>               2.5 % 97.5 %
#> e 1.429248 1.126288 1.8137

#case-time-control
ctcfit <- CXO_tc_wt(casetimecontrols, exposure = ex, event = Event, Id=Id)   
exp(cbind(coef(ctcfit), confint(ctcfit)))
#>                      2.5 %   97.5 %
#> ex1    0.9828992 0.7144712 1.352176
#> ex_tc1 1.5681218 1.2684209 1.938636
```

Other functions include

- ‘mhor’ to produce Mantel-Haenszel (MH) Odds Ratios
- ‘SCL_bias’ to estimate the bias in (unweighted) conditional logistic
  regression compared with MH ORs

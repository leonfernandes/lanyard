
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lanyard

<!-- badges: start -->

<!-- badges: end -->

The goal of lanyard is to provide goodness-of-fit tests as
[yardstick](https://yardstick.tidymodels.org/) metrics.

## Installation

You can install the development version of lanyard like so:

``` r
# Install from github
pak::pkg_install("leonfernandes/lanyard")
```

## Example

As an example, you can calculate the autocovariance/autocorrelation
using the `acf_metric` function.

``` r
library(lanyard)
data <- data.frame(t = rnorm(100), e = rnorm(100)) # simulate data
acf_metric(data, t, e) # get acf
#> # A tibble: 99 × 3
#>      lag autocovariance autocorrelation
#>    <int>          <dbl>           <dbl>
#>  1     1        -0.103         -0.0669 
#>  2     2        -0.162         -0.106  
#>  3     3         0.0654         0.0426 
#>  4     4        -0.345         -0.225  
#>  5     5         0.0681         0.0443 
#>  6     6         0.0779         0.0507 
#>  7     7        -0.0148        -0.00966
#>  8     8        -0.273         -0.178  
#>  9     9        -0.102         -0.0667 
#> 10    10         0.0903         0.0589 
#> # ℹ 89 more rows
```

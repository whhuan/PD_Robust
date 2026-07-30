# Diagnose propensity-score covariate balance

Fits the propensity-score model internally on baseline observations,
clips every estimated propensity score to `[0.01, 0.99]`, creates
ordinary inverse-probability-of-treatment weights, and evaluates balance
using the original pooled and weighted-ESS SMD denominators.

## Usage

``` r
PSDiag(data, ps_fo)
```

## Arguments

- data:

  A standardized `pd_data` object.

- ps_fo:

  Propensity-score formula.

## Value

A `PSDiag` object containing SMDs, weights, predictions, and a plot.

## Examples

``` r
# \donttest{
data("BiSample", package = "PDRobust")
map <- Mapping(
  baseline_time = 0, cutoff_time = 2,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"), y_type = "B"
)
pd_dat <- DataStandard(BiSample, map)
result <- PSDiag(pd_dat, A ~ X1 + X2 + X4)
result$smd_after
#>           X1           X2           X4 
#>  0.220926393 -0.085729356 -0.001878587 
# }
```

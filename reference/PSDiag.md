# Evaluate how well a propensity score model performs

Calculates the standardized mean difference (SMD) for each covariate
before and after propensity score weighting.

## Usage

``` r
PSDiag(data, ps_fo)
```

## Arguments

- data:

  A standardized `pd_data` object.

- ps_fo:

  propensity score model formula

## Value

A `PSDiag` object containing three-decimal SMD summaries and a plot;
propensity scores and weights retain full precision.

## Details

`PSDiag()` fits the propensity score model using baseline observations,
constrains the estimated propensity scores to `[0.01, 0.99]`, and
constructs ordinary inverse-probability-of-treatment weights. Comparing
each covariate's SMD before and after weighting assesses covariate
balance and indicates how well the fitted propensity score model
balances the treatment groups.

## Examples

``` r
# \donttest{
data("BiSample", package = "PDRobust")
map <- Mapping(
  id = "id", time = "time", treatment = "A",
  survival = "S", outcome = "Y",
  baseline_time = 0, cutoff_time = 2,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"), y_type = "B"
)
pd_dat <- DataStandard(BiSample, map)
result <- PSDiag(pd_dat, A ~ X1 + X2 + X4)
result$smd_after
#>     X1     X2     X4 
#> -0.197 -0.047  0.056 
# }
```

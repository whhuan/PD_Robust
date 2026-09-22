# Evaluate how well a propensity score model performs

Calculates the standardized mean difference (SMD) for each covariate
before and after propensity score weighting.

## Usage

``` r
PSDiag(data, ps_fo)
```

## Arguments

- data:

  Data prepared by
  [`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md).

- ps_fo:

  propensity score model formula

## Value

A `PSDiag` object containing SMDs before and after weighting, the
estimated propensity scores and weights, and a balance plot. SMDs are
rounded to three decimal places.

## Details

`PSDiag()` fits the propensity score model using baseline observations
and uses inverse-probability-of-treatment weighting to make the
treatment groups more comparable. It limits estimated probabilities to
`[0.01, 0.99]` to avoid extremely large weights. A smaller absolute SMD
after weighting indicates better balance for that covariate.

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

# Summarize cutoff covariates in the always-survivor principal stratum

Restores the original weighted intercept-only quantile-regression
algorithm. For each nonbinary mapped interest variable,
`quantreg::rq(variable ~ 1, weights = K_p0, tau = quantile_level)` is
fitted on cutoff rows.

## Usage

``` r
QR(data, prin_fo, quantile_level = 0.5)
```

## Arguments

- data:

  A standardized `pd_data` object.

- prin_fo:

  Principal-score formula.

- quantile_level:

  Quantile probabilities strictly between zero and one.

## Value

A `QR` object containing weighted means and quantiles.

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
result <- QR(
  pd_dat,
  S ~ X1 + X2 + X4 + A + time,
  quantile_level = c(0.25, 0.5, 0.75)
)
result$mean
#>        X1        X2 
#> 0.3586368 0.1272697 
# }
```

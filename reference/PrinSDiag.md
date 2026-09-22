# Evaluate covariate balance for the principal score model

Calculates a standardized balance statistic for each numeric covariate
at the cutoff time after accounting for treatment assignment and
estimated survival. Values nearer zero indicate better balance between
the weighted treatment groups.

## Usage

``` r
PrinSDiag(data, ps_fo, prin_fo)
```

## Arguments

- data:

  Data prepared by
  [`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md).

- ps_fo:

  propensity score model formula

- prin_fo:

  principal score model formula

## Value

A `PrinSDiag` object containing the standardized balance statistics,
estimated probabilities, and a diagnostic plot. Balance statistics are
rounded to three decimal places.

## Details

The function fits both the propensity score and principal score models.
It uses all observed times from baseline through cutoff to estimate
cumulative survival probabilities, limits propensity scores to
`[0.01, 0.99]`, and then calculates the balance statistics at the cutoff
time.

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
result <- PrinSDiag(
  pd_dat,
  A ~ X1 + X2 + X4,
  S ~ X1 + X2 + X4 + A + time
)
result$statistics
#>     X1     X2     X4 
#> -1.182  0.869 -0.074 
# }
```

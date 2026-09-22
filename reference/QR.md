# Summary statistics of covariates within the always-survivor principal stratum

Estimates the user-specified quantile for continuous covariates and the
mean of covariates for subjects within the always-survivor principal
stratum.

## Usage

``` r
QR(data, prin_fo, quantile_level = 0.5)
```

## Arguments

- data:

  Data prepared by
  [`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md).

- prin_fo:

  principal score model formula

- quantile_level:

  One or more quantiles to estimate, expressed as probabilities strictly
  between `0` and `1`. Defaults to the median (`0.5`).

## Value

A `QR` object containing the weighted means, requested quantiles,
variable-type indicators, and weights. Reported means and quantiles are
rounded to three decimal places.

## Details

`QR()` uses estimated survival probabilities under treatment `0` to
weight the numeric variables listed in `interest_vars`. It reports a
weighted mean for every variable. For variables with more than two
observed values, it also reports the requested weighted quantiles.
Variables with no more than two observed values are treated as binary
and receive a mean but no quantile.

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
result <- QR(
  pd_dat,
  S ~ X1 + X2 + X4 + A + time,
  quantile_level = c(0.25, 0.5, 0.75)
)
result$mean
#>    X1    X2 
#> 0.140 0.141 
# }
```

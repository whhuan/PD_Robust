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

  A standardized `pd_data` object.

- prin_fo:

  principal score model formula

- quantile_level:

  Quantile probabilities strictly between zero and one.

## Value

A `QR` object containing three-decimal weighted means and quantiles;
principal-score weights retain full precision.

## Details

`QR()` estimates cumulative principal scores under treatment level `0`
and uses their cutoff values as weights for the mapped numeric interest
variables. It reports weighted means for all such variables and
estimates the requested quantiles for variables with more than two
observed values using weighted intercept-only quantile regression.
Variables with no more than two observed values are treated as binary
and receive a weighted mean but no quantile estimate.

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

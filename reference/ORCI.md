# Estimate treatment-group-specific survival odds ratios at cutoff

Fits the supplied logistic model among subjects in the selected
treatment group at the mapped cutoff time.

## Usage

``` r
ORCI(data, fomula, a, conf_level = 0.95)
```

## Arguments

- data:

  A standardized `pd_data` object.

- fomula:

  Logistic-regression formula.

- a:

  Required cutoff treatment group, exactly `0` or `1`.

- conf_level:

  Confidence level.

## Value

An `odds_ratios` object containing three-decimal odds-ratio summaries, a
full-precision fitted model, model diagnostics, and a plot.

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
result <- ORCI(
  pd_dat, S ~ X1 + X2 + X4, a = 0
)
result$forestplotdat
#>    covname estcoef lowerbd upperbd
#> X1      X1   1.788   1.021   3.132
#> X2      X2   1.834   0.988   3.404
#> X4      X4   2.028   0.677   6.076
# }
```

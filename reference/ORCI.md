# Estimate treatment-group-specific survival odds ratios at cutoff

Fits the supplied logistic model among subjects in the selected
treatment group at the mapped cutoff time.

## Usage

``` r
ORCI(data, fomula, treatment_group = 0, conf_level = 0.95)
```

## Arguments

- data:

  A standardized `pd_data` object.

- fomula:

  Logistic-regression formula.

- treatment_group:

  Cutoff treatment group, exactly `0` or `1`.

- conf_level:

  Confidence level.

## Value

An `odds_ratios` object containing estimates, model, and plot.

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
result <- ORCI(
  pd_dat, S ~ X1 + X2 + X4, treatment_group = 0
)
result$forestplotdat
#>    covname    estcoef   lowerbd    upperbd
#> X1      X1  1.3632587 0.4255882   4.366838
#> X2      X2 15.0439260 1.3934636 162.415230
#> X4      X4  0.9979709 0.1218108   8.176174
# }
```

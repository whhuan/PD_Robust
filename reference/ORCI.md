# Estimate covariate associations with survival at the cutoff time

Estimates odds ratios with confidence intervals for associations between
covariates and survival at the cutoff time within a selected treatment
group.

## Usage

``` r
ORCI(data, formula, a, conf_level = 0.95)
```

## Arguments

- data:

  Data prepared by
  [`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md).

- formula:

  A logistic regression formula with the survival variable on the
  left-hand side and the covariates of interest on the right-hand side.

- a:

  The treatment group to analyze at the cutoff time, either `0` or `1`.

- conf_level:

  The confidence level, expressed as a single number between `0` and
  `1`. Defaults to `0.95`.

## Value

An `odds_ratios` object containing odds-ratio estimates and confidence
intervals, the fitted model, model-checking information, and a forest
plot. Reported estimates are rounded to three decimal places.

## Details

`ORCI()` fits the supplied logistic regression model using observations
from treatment group `a` at the cutoff time. It reports an odds ratio
and Wald confidence interval for every non-intercept coefficient that
can be estimated. Covariates are not selected according to statistical
significance.

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

# Estimate propensity scores

Fits a logistic propensity-score model on baseline observations from
`fit_dat` and predicts on every row of `pred_dat`. The model is refitted
on every call; no fitted object or cache is retained.

## Usage

``` r
PSPred(ps_fo, fit_dat, pred_dat, mapping, ...)
```

## Arguments

- ps_fo:

  Propensity-score formula.

- fit_dat:

  Data used to fit the model.

- pred_dat:

  Data on which to predict.

- mapping:

  A `pd_mapping` object. It supplies column names and `baseline_time`
  only; it never supplies data.

- ...:

  Additional arguments passed to
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

## Value

A numeric vector of class `pd_prediction` with length `nrow(pred_dat)`,
rounded to three decimal places after prediction.

## Examples

``` r
data("BiSample", package = "PDRobust")
map <- Mapping(
  id = "id", time = "time", treatment = "A",
  survival = "S", outcome = "Y",
  baseline_time = 0, cutoff_time = 2,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"), y_type = "B"
)
pd_dat <- DataStandard(BiSample, map)
ps <- PSPred(A ~ X1 + X2 + X4, pd_dat, pd_dat, map)
head(ps)
#> [1] 0.971 0.971 0.971 0.933 0.933 0.933
```

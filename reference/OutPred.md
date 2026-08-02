# Estimate outcome predictions

Refits the outcome model on every call and predicts on all rows of
`pred_dat`. During prediction the mapped treatment column is set to `a`
and the mapped survival column is set to one, exactly as in the original
method.

## Usage

``` r
OutPred(out_fo, fit_dat, pred_dat, a, mapping, ...)
```

## Arguments

- out_fo:

  Outcome-model formula.

- fit_dat:

  Data used to fit the outcome model.

- pred_dat:

  Data on which to predict.

- a:

  Treatment value, either `0` or `1`.

- mapping:

  A `pd_mapping` object. `mapping$y_type` selects linear or logistic
  regression.

- ...:

  Additional arguments passed to
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
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
mu1 <- OutPred(Y ~ X1 + X2 + A + S, pd_dat, pd_dat, a = 1, mapping = map)
head(mu1)
#> [1] 0.230 0.230 0.230 0.222 0.222 0.222
```

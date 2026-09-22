# Estimate outcome predictions

Fits the outcome mean model and predicts each row of `pred_dat` under
treatment `a` and survival status `1`. The model is fitted again each
time the function is called.

## Usage

``` r
OutPred(out_fo, fit_dat, pred_dat, a, mapping, ...)
```

## Arguments

- out_fo:

  outcome mean model formula

- fit_dat:

  A data frame containing the observations used to fit the outcome mean
  model.

- pred_dat:

  A data frame containing the observations for which outcome predictions
  are requested.

- a:

  The treatment level under which outcomes are predicted, either `0` or
  `1`.

- mapping:

  A `pd_mapping` object. Its outcome type determines whether the
  function uses linear regression or logistic regression.

- ...:

  Additional arguments passed to
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

## Value

A numeric vector of predicted outcome means, one for each row of
`pred_dat`, rounded to three decimal places.

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

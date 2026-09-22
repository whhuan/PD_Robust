# Estimate propensity scores

Fits a logistic model using baseline observations from `fit_dat` and
returns each row's estimated probability of receiving treatment `1` in
`pred_dat`. The model is fitted again each time the function is called.

## Usage

``` r
PSPred(ps_fo, fit_dat, pred_dat, mapping, ...)
```

## Arguments

- ps_fo:

  propensity score model formula

- fit_dat:

  A data frame containing the baseline observations used to fit the
  model.

- pred_dat:

  A data frame containing the observations for which propensity scores
  are requested.

- mapping:

  A `pd_mapping` object that identifies the treatment and time columns
  and the baseline time.

- ...:

  Additional arguments passed to
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

## Value

A numeric vector of propensity scores, one for each row of `pred_dat`,
rounded to three decimal places.

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
#> [1] 0.942 0.942 0.942 0.863 0.863 0.863
```

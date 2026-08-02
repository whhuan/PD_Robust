# Estimate cumulative principal scores

Fits the principal-score model and returns cumulative survival
probabilities for all rows of `pred_dat`. All actual observed times from
baseline through cutoff are used. The model is refitted on every call.

## Usage

``` r
PrinPred(prin_fo, fit_dat, pred_dat, a, mapping, ...)
```

## Arguments

- prin_fo:

  Principal-score formula.

- fit_dat:

  Data used to fit the model.

- pred_dat:

  Data on which to predict cumulative scores.

- a:

  Treatment level for principal-score prediction, either `0` or `1`.

- mapping:

  A `pd_mapping` object.

- ...:

  Additional arguments passed to
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

## Value

A numeric vector of class `pd_prediction` with length `nrow(pred_dat)`,
rounded to three decimal places after cumulative probabilities have been
calculated.

## Details

When multiple observed time points exist, baseline rows are assigned an
at-risk indicator of zero and each post-baseline row is included only
when the subject survived at the immediately preceding observed time.
When the analysis contains only one observed time point, no at-risk
indicator is constructed and all complete observations at that time are
used for fitting.

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
score0 <- PrinPred(
  S ~ X1 + X2 + X4 + A + time,
  pd_dat, pd_dat, a = 0, mapping = map
)
head(score0)
#> [1] 1.000 0.920 0.814 1.000 0.947 0.873
```

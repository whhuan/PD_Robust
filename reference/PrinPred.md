# Estimate cumulative principal scores

Fits the principal score model and returns each row's estimated
probability of surviving from baseline through its observed time under
treatment `a`. All observed times from baseline through cutoff are used,
and the model is fitted again each time the function is called.

## Usage

``` r
PrinPred(prin_fo, fit_dat, pred_dat, a, mapping, ...)
```

## Arguments

- prin_fo:

  principal score model formula

- fit_dat:

  A data frame containing the observations used to fit the model.

- pred_dat:

  A data frame containing the observations for which cumulative survival
  probabilities are requested.

- a:

  The treatment level under which survival probabilities are predicted,
  either `0` or `1`.

- mapping:

  A `pd_mapping` object that identifies the variables and analysis
  times.

- ...:

  Additional arguments passed to
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

## Value

A numeric vector of cumulative survival probabilities, one for each row
of `pred_dat`, rounded to three decimal places.

## Details

When the data contain multiple times, each post-baseline observation is
used to model the next survival step only if the subject was alive at
the previous observed time. If the data contain only one observed time,
all complete observations at that time are used.

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

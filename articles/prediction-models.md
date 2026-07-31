# Independent prediction models

[`PSPred()`](https://whhuan.github.io/PD_Robust/reference/PSPred.md),
[`PrinPred()`](https://whhuan.github.io/PD_Robust/reference/PrinPred.md),
and
[`OutPred()`](https://whhuan.github.io/PD_Robust/reference/OutPred.md)
fit and predict in one call. They do not return or cache fitted model
objects. Each result is a row-aligned numeric vector with class
`pd_prediction`.

``` r

library(PDRobust)
data("BiSample", package = "PDRobust")
raw <- BiSample
map <- Mapping(
  id = "id",
  time = "time",
  treatment = "A",
  survival = "S",
  outcome = "Y",
  baseline_time = 0,
  cutoff_time = 2,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"),
  y_type = "B"
)
pd_data <- DataStandard(raw, map)
map <- attr(pd_data, "pd_mapping")
```

## Propensity prediction

``` r

ps <- PSPred(
  A ~ X1 + X2 + X4,
  fit_dat = pd_data,
  pred_dat = pd_data,
  mapping = map
)
head(ps)
```

The logistic model is fitted only on baseline observations, then used to
predict every row of `pred_dat`.

## Cumulative principal-score prediction

``` r

p0 <- PrinPred(
  S ~ X1 + X2 + X4 + A + time,
  fit_dat = pd_data,
  pred_dat = pd_data,
  a = 0,
  mapping = map
)
p1 <- PrinPred(
  S ~ X1 + X2 + X4 + A + time,
  fit_dat = pd_data,
  pred_dat = pd_data,
  a = 1,
  mapping = map
)
head(cbind(pd_data[c("id", "time")], p0, p1))
```

For multiple observed times, baseline rows are excluded from the
principal model’s risk set. A post-baseline row is fitted only when
survival at the immediately preceding observed time equals one.
Conditional predictions at baseline are set to one and accumulated
within subject over the complete baseline-to-cutoff grid. For a single
observed time, no at-risk indicator is constructed and all complete rows
are used.

Separation, aliased nuisance coefficients, or extreme fitted
probabilities may produce warnings. They do not invalidate a prediction
when the fitted model still returns a complete finite vector. Missing,
non-finite, or row-misaligned predictions remain errors. Direct
prediction functions report one normalized warning per fit.
[`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md),
[`HTEAllT()`](https://whhuan.github.io/PD_Robust/reference/HTEAllT.md),
and [`SA()`](https://whhuan.github.io/PD_Robust/reference/SA.md)
consolidate repeated point-estimate warnings and retain bootstrap
warnings in returned diagnostics. Within one analysis sample, the same
fitted principal-score or outcome model is reused for its two
counterfactual treatment predictions when its fitting rows and formula
are identical.

Public prediction vectors are rounded to three decimal places. Analysis
functions call full-precision internal implementations, so this display
rounding never enters weights, estimating equations, optimization,
bootstrap replicates, or confidence-interval calculations.

## Outcome prediction

``` r

fit_data <- pd_data[pd_data$S == 1, , drop = FALSE]
mu0 <- OutPred(
  Y ~ X1 + X2 + A,
  fit_dat = fit_data,
  pred_dat = pd_data,
  a = 0,
  mapping = map
)
mu1 <- OutPred(
  Y ~ X1 + X2 + A,
  fit_dat = fit_data,
  pred_dat = pd_data,
  a = 1,
  mapping = map
)
head(cbind(mu0, mu1))
```

[`OutPred()`](https://whhuan.github.io/PD_Robust/reference/OutPred.md)
removes fitting rows with missing outcomes, sets treatment to the
requested value and survival to one in prediction data, and uses either
a linear or logistic outcome model according to `mapping$y_type`.

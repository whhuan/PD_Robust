# Prediction models

## Introduction

The complete workflow is illustrated as follows. This article focuses on
the package’s prediction models, including three nuisance models
[`PSPred()`](https://whhuan.github.io/PD_Robust/reference/PSPred.md),
[`PrinPred()`](https://whhuan.github.io/PD_Robust/reference/PrinPred.md)
and
[`OutPred()`](https://whhuan.github.io/PD_Robust/reference/OutPred.md).

``` text
data -> Mapping() -> DataCheck() -> DataStandard()
     -> prediction / diagnostic / analysis functions
```

``` r

library(PDRobust)
data("BiSample", package = "PDRobust")

map <- Mapping(
  id = "id",
  time = "time",
  treatment = "A",
  survival = "S",
  outcome = "Y",
  baseline_time = 0,
  cutoff_time = 2,
  covariates = c("X1", "X2", "X3", "X4", "X5","X6"),
  interest_vars = c("X1", "X2"),
  y_type = "B"
)

pd_data <- DataStandard(BiSample, map)
```

``` r

head(pd_data)
#>   id time    Pi S1 S0 S A Y1 Y0 Y    X1     X2     X3 X4 X5 X6
#> 1  1    0 0.987  1  1 1 1  0  1 0 1.479 -0.168  0.873  0  1  1
#> 2  1    1 0.987  1  1 1 1  0  0 0 1.479 -0.168  0.873  0  1  1
#> 3  1    2 0.987  1  1 1 1  0  0 0 1.479 -0.168  0.873  0  1  1
#> 4  2    0 0.777  1  1 1 1  0  0 0 0.267  0.350 -1.438  1  1  1
#> 5  2    1 0.777  1  1 1 1  0  0 0 0.267  0.350 -1.438  1  1  1
#> 6  2    2 0.777  1  1 1 1  0  0 0 0.267  0.350 -1.438  1  1  1
```

``` r

ps_fo <- A ~ X1 + X3 + X4 + X5 + X6
prin_fo <- S ~ (X1 + X3 + X4 + X5 + X6 ) * A
out_fo <- Y ~ (X1 + X3 + X4 + X5 + X6) * A 
```

## Propensity score model

[`PSPred()`](https://whhuan.github.io/PD_Robust/reference/PSPred.md)
returns a numeric vector of propensity score estimation, which is
defined as the conditional probability of receiving treatment given the
specified covariates. The argument `ps_fo` specified the propensity
score model formula. The argument `fit_dat` provides the dataset to fit
the mode, whereas `pred_dat` is for propensity score predictions. The
argument mapping is the original mapping object at the beginning of the
workflow and supplies the relevant structural variable definitions.

The logistic model is fitted only on baseline observations, then used to
predict every row of `pred_dat`. Additional arguments supplied through
`...` are forwarded to the underlying
[`glm()`](https://rdrr.io/r/stats/glm.html) call, allowing users to
customize the model-fitting procedure.

``` r

ps <- PSPred(
  ps_fo = ps_fo,
  fit_dat = pd_data,
  pred_dat = pd_data,
  mapping = map
)
head(ps)
#> [1] 0.987 0.987 0.987 0.873 0.873 0.873
```

## Principal score model

[`PrinPred()`](https://whhuan.github.io/PD_Robust/reference/PrinPred.md)
estimates cumulative principal score under a specified treatment level
and returns a numeric vector. For longitudinal data with multiple time
points, the cumulative score combines the estimated survival
probabilities across the observed post-baseline times. For data
containing a single observed time point, the model is fitted using the
complete dataset.

The argument `prin_fo`, `fit_dat`, and `pred_dat` specify the formula,
data used to fit the model and the observations where predictions are
generated, respectively. The argument `mapping` is the original argument
mapping. The argument `a` specifies the treatment group under which the
principal scores are predicted.

``` r

p1 <- PrinPred(
  prin_fo = prin_fo,
  fit_dat = pd_data,
  pred_dat = pd_data,
  a = 1,
  mapping = map
)
head(p1)
#> [1] 1.000 0.974 0.949 1.000 0.953 0.909
```

## Outcome prediction model

[`OutPred()`](https://whhuan.github.io/PD_Robust/reference/OutPred.md)
estimates potential outcomes under a specified treatment level and
returns the predictions as a row-aligned numeric vector.

The argument `out_fo`, `fit_dat`, `pre_dat` is the outcome model
formula, data used to fit the model and data in which predictions are
generated, respectively. The argument `mapping` is the original argument
mapping. The argument `a` specifies the treatment group under which the
outcomes are predicted.

The model-fitting procedure depends on the outcome type specified in
mapping. For a continuous outcome,
[`OutPred()`](https://whhuan.github.io/PD_Robust/reference/OutPred.md)
fits a linear regression model and returns the conditional means. For a
binary outcome, it fits a logistic regression model.

``` r

mu0 <- OutPred(
  out_fo = out_fo,
  fit_dat = pd_data,
  pred_dat = pd_data,
  a = 0,
  mapping = map
)

head(mu0)
#> [1] 0.100 0.100 0.100 0.247 0.247 0.247
```

**Tips:**

Model-fitting issues such as complete or quasi-complete separation,
aliased nuisance-model coefficients, or extreme fitted probabilities may
generate warnings. These conditions do not necessarily invalidate the
predictions, provided that the fitted model returns a complete, finite,
and correctly aligned prediction vector.

In contrast, missing, non-finite, or row-misaligned predictions are
treated as errors. Each direct prediction function reports a single
standardized warning for each model fit.

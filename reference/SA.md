# Perform outcome-noise sensitivity analysis

Restores the original sensitivity-analysis equations and variance
definition. At each actual observed time from baseline through cutoff,
the perturbation variance is the ordinary variance of all observed
outcomes at that time. Both cutoff treatment groups enter the estimating
equations.

## Usage

``` r
SA(data, ps_fo, prin_fo, out_fo, ratiovec = c(0, 0.05, 0.1))
```

## Arguments

- data:

  A standardized continuous- or binary-outcome `pd_data` object.

- ps_fo:

  Propensity-score formula.

- prin_fo:

  Principal-score formula.

- out_fo:

  Outcome-model formula.

- ratiovec:

  Finite nonnegative outcome-variance ratios.

## Value

An `SA` object containing rounded tidy and wide estimates,
full-precision estimating diagnostics, consolidated warnings, and plots.

## Details

Continuous outcomes retain the original additive-noise implementation:
the perturbed outcomes are used both to refit the linear outcome model
and in the estimating equation. For binary outcomes, additive
perturbations are applied to the estimating-equation outcome while the
logistic nuisance model is fitted to the original 0/1 outcomes. This
keeps the outcome model binomial rather than fitting a logistic model to
invalid pseudo-responses. Binary HTE coefficients use the same
bounded-link estimating equation as
[`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md).

All three prediction models are refitted internally; no fitted model is
cached or reused across calls. Within one scenario, a model fitted to
the same rows and formula is reused only to obtain the two
counterfactual treatment predictions.

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
result <- SA(
  pd_dat,
  A ~ X1 + X2 + X4,
  S ~ X1 + X2 + X4 + A + time,
  Y ~ X1 + X2 + A,
  ratiovec = c(0, 0.05)
)
head(result$data)
#>   ratio time      term estimate
#> 1     0    0 Intercept    0.066
#> 2     0    0        X1    0.035
#> 3     0    0        X2    0.053
#> 4     0    1 Intercept    0.220
#> 5     0    1        X1   -0.039
#> 6     0    1        X2   -0.091
# }
```

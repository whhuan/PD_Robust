# Sensitivity analysis of outcome mean model misspecification

Evaluates the sensitivity of time-specific heterogeneous treatment
effect estimates to a specified random outcome-noise perturbation
intended to probe outcome mean model misspecification. At each observed
analysis time, the perturbation variance is based on the ordinary
variance of the observed outcomes, and both cutoff treatment groups
contribute to the estimating equations.

## Usage

``` r
SA(data, ps_fo, prin_fo, out_fo, ratiovec = c(0, 0.05, 0.1))
```

## Arguments

- data:

  A standardized continuous- or binary-outcome `pd_data` object.

- ps_fo:

  propensity score model formula

- prin_fo:

  principal score model formula

- out_fo:

  outcome mean model formula

- ratiovec:

  Finite nonnegative outcome-variance ratios.

## Value

An `SA` object containing rounded tidy and wide estimates,
full-precision estimating diagnostics, consolidated warnings, and plots.

## Details

`SA()` adds mean-zero random noise to the outcome for every combination
of observed analysis time and user-specified variance ratio. The noise
variance equals the specified ratio multiplied by the ordinary variance
of the observed outcomes at that time, after which the function
estimates the time-specific heterogeneous treatment effects.

For continuous outcomes, the perturbed outcomes are used both to refit
the linear outcome mean model and in the estimating equation. For binary
outcomes, the logistic outcome mean model is fitted to the original
binary outcomes, while the perturbed outcomes enter the estimating
equation; the heterogeneous treatment effect coefficients use the same
bounded-link estimating equation as
[`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md).

The propensity score, principal score, and outcome mean models are
refitted internally as required by each scenario. This procedure
evaluates sensitivity to the specified random outcome-noise
perturbation, not to arbitrary forms of model misspecification or
violations of the causal identifying assumptions. Setting an R random
seed before calling `SA()` makes the perturbations reproducible.

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
set.seed(20260912)
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

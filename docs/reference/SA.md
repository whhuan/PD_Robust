<div id="main" class="col-md-9" role="main">

# Perform outcome-noise sensitivity analysis

<div class="ref-description section level2">

Restores the original sensitivity-analysis equations and variance
definition. At each actual observed time from baseline through cutoff,
the perturbation variance is the ordinary variance of all observed
outcomes at that time. Both cutoff treatment groups enter the estimating
equations.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
SA(data, ps_fo, prin_fo, out_fo, ratiovec = c(0, 0.05, 0.1))
```

</div>

</div>

<div class="section level2">

## Arguments

-   data:

    A standardized continuous- or binary-outcome `pd_data` object.

-   ps\_fo:

    Propensity-score formula.

-   prin\_fo:

    Principal-score formula.

-   out\_fo:

    Outcome-model formula.

-   ratiovec:

    Finite nonnegative outcome-variance ratios.

</div>

<div class="section level2">

## Value

An `SA` object containing rounded tidy and wide estimates,
full-precision estimating diagnostics, consolidated warnings, and plots.

</div>

<div class="section level2">

## Details

Continuous outcomes retain the original additive-noise implementation:
the perturbed outcomes are used both to refit the linear outcome model
and in the estimating equation. For binary outcomes, additive
perturbations are applied to the estimating-equation outcome while the
logistic nuisance model is fitted to the original 0/1 outcomes. This
keeps the outcome model binomial rather than fitting a logistic model to
invalid pseudo-responses. Binary HTE coefficients use the same
bounded-link estimating equation as `HTESepT()`.

This analysis measures sensitivity to the specified random outcome-noise
perturbation. It does not identify the direction or magnitude of
arbitrary model misspecification or test the causal identifying
assumptions. Set an R random seed before calling `SA()` to reproduce its
perturbations.

All three prediction models are refitted internally; no fitted model is
cached or reused across calls. Within one scenario, a model fitted to
the same rows and formula is reused only to obtain the two
counterfactual treatment predictions.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

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

</div>

</div>

</div>

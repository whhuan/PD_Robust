# Sensitivity analysis of outcome mean model misspecification

Examines how time-specific heterogeneous treatment effect estimates
change when random noise is added to the outcome. The amount of noise is
determined from the observed outcome variance at each analysis time, and
subjects from both treatment groups at the cutoff contribute to effect
estimation.

## Usage

``` r
SA(data, ps_fo, prin_fo, out_fo, ratiovec = c(0, 0.05, 0.1))
```

## Arguments

- data:

  Continuous- or binary-outcome data prepared by
  [`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md).

- ps_fo:

  propensity score model formula

- prin_fo:

  principal score model formula

- out_fo:

  outcome mean model formula

- ratiovec:

  One or more nonnegative numbers that set the added-noise variance as a
  proportion of the observed outcome variance. Use `0` for a scenario
  with no added noise.

## Value

An `SA` object containing estimates for every analysis time and noise
level, model-checking information, warnings, and plots. Displayed
estimates are rounded to three decimal places.

## Details

For each observed analysis time and each value in `ratiovec`, `SA()`
adds mean-zero random noise whose variance equals that value multiplied
by the observed outcome variance. It then re-estimates the heterogeneous
treatment effect for that time.

For continuous outcomes, the perturbed outcomes are used both to refit
the outcome mean model and to estimate the treatment effect. For binary
outcomes, the outcome mean model is fitted to the original binary
outcomes, while the perturbed outcomes are used only during effect
estimation. Binary treatment effects use the same bounded scale as
[`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md).

The three models are fitted again as needed for each scenario. The
results show sensitivity to this particular form of random outcome
noise; they do not cover every possible model error or violation of the
causal assumptions. Set an R random seed before calling `SA()` to
reproduce the same noise.

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

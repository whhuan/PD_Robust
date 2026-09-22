# Joint estimation of heterogeneous treatment effects across time

Performs joint longitudinal analysis of heterogeneous treatment effects
across all time points with time included as a covariate.

## Usage

``` r
HTEAllT(
  data,
  ps_fo,
  prin_fo,
  out_fo,
  B,
  conf_level = 0.95,
  max_attempts = NULL,
  verbose = TRUE,
  progress_callback = NULL
)
```

## Arguments

- data:

  Data prepared by
  [`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md).

- ps_fo:

  propensity score model formula

- prin_fo:

  principal score model formula

- out_fo:

  outcome mean model formula

- B:

  Number of bootstrap replications. Use `0` for point estimates only.

- conf_level:

  The confidence level for Wald intervals calculated from bootstrap
  standard errors.

- max_attempts:

  The maximum number of resampling attempts allowed to obtain `B`
  successful bootstrap replications. Defaults to `10B`. Resampling stops
  once `B` successful replications are obtained or when the maximum
  number of attempts is reached, whichever occurs first. Thus, fewer
  than `B` successful replications may be returned if the maximum number
  of attempts is reached.

- verbose:

  If `TRUE`, print bootstrap progress messages.

- progress_callback:

  An optional function for receiving bootstrap progress updates. It is
  called before model fitting, after the point estimate, after every
  bootstrap attempt, and when bootstrapping finishes. Each update is a
  named list containing `stage`, `successful`, `requested`, `attempts`,
  `max_attempts`, `failed_attempts`, `complete`, `elapsed_seconds`, and
  `updated_at`. If the callback produces an error, the function warns
  once and stops sending updates; the statistical analysis continues.

## Value

A `pd_hte_pooled` object containing the jointly estimated trajectory,
the analysis times, confidence intervals when `B > 0`, model-checking
information, and a summary of successful and failed bootstrap attempts.
`time_effect_estimable` indicates whether the data allowed a time effect
to be included. Displayed estimates are rounded to three decimal places;
`boot_mat` stores the unrounded bootstrap coefficients.

## Details

`HTEAllT()` combines predictions from the propensity score, principal
score, and outcome mean models to form the data used for effect
estimation. It then estimates one treatment-effect trajectory across all
observed times and, when requested, uses bootstrap resampling to
calculate confidence intervals.

## Numerical safeguards

Propensity scores are limited to `[0.01, 0.99]`. Their product with
estimated survival probabilities under treatment `1` is limited to
`[0.005, 0.995]` when effects are estimated. These limits prevent
division by probabilities very close to zero, but they can affect the
estimates and do not demonstrate adequate overlap or validate the causal
assumptions. Review the returned model information, especially when few
subjects remain at risk.

## Treatment coding

The implemented estimator uses treatment `1` as the survival-favorable
arm: potential survival satisfies \\S^1 \ge S^0\\ at cutoff. Its
always-survivor principal score is therefore the survival probability
under treatment `0`. If the survival-favorable arm is coded as `0` in
the raw data, recode the raw treatment as `1 - A` before mapping and
standardizing. To report the original contrast, negate the package
estimate and transform an interval `[lower, upper]` to
`[-upper, -lower]`.
[`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md)
does not infer or reverse treatment coding.

## See also

[PDRobust-package](https://whhuan.github.io/PD_Robust/reference/PDRobust-package.md),
[pd_methods](https://whhuan.github.io/PD_Robust/reference/pd_methods.md)

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
fit <- HTEAllT(
  pd_dat,
  A ~ X1 + X2 + X4,
  S ~ X1 + X2 + X4 + A + time,
  Y ~ X1 + X2 + A,
  B = 0
)
fit$summary
#>          term estimate SD LowerBound UpperBound
#> 1   Intercept    0.114 NA         NA         NA
#> 2          X1    0.156 NA         NA         NA
#> 3          X2   -0.025 NA         NA         NA
#> 4 Time Effect   -0.029 NA         NA         NA
# }
```

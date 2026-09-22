# Separate estimation of heterogeneous treatment effects by time point

Performs separate analyses of heterogeneous treatment effects at each
selected time point.

## Usage

``` r
HTESepT(
  data,
  ps_fo,
  prin_fo,
  out_fo,
  target_time,
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

- target_time:

  A non-empty numeric vector containing timepoints of interest in
  standardized form. Baseline is allowed.

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

A `pd_hte_timevarying` object containing the estimate for each requested
time, confidence intervals when `B > 0`, model-checking information, and
a summary of successful and failed bootstrap attempts. Displayed
estimates are rounded to three decimal places; `boot_mat` stores the
unrounded bootstrap coefficients.

## Details

`HTESepT()` combines predictions from the propensity score, principal
score, and outcome mean models to form the data used for effect
estimation. It fits a separate treatment-effect model at each selected
time and, when requested, uses bootstrap resampling to calculate
confidence intervals.

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
fit <- HTESepT(
  pd_dat,
  A ~ X1 + X2 + X4,
  S ~ X1 + X2 + X4 + A + time,
  Y ~ X1 + X2 + A,
  target_time = c(0, 2), B = 0
)
fit$summary
#>   time covariate estimate SD LowerBound UpperBound
#> 1    0 Intercept    0.066 NA         NA         NA
#> 2    0        X1    0.035 NA         NA         NA
#> 3    0        X2    0.053 NA         NA         NA
#> 4    2 Intercept   -0.028 NA         NA         NA
#> 5    2        X1    0.516 NA         NA         NA
#> 6    2        X2   -0.036 NA         NA         NA
# }
```

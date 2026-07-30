# Estimate time-specific heterogeneous treatment effects

`target_time` is defined only for `HTESepT()`. It may include the mapped
baseline and controls only the outcome-analysis times reported by this
function. Principal scores are nevertheless accumulated over every
actual observed time from baseline through cutoff because the principal
stratum is defined at the cutoff.

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
  verbose = TRUE
)
```

## Arguments

- data:

  A standardized `pd_data` object returned by
  [`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md).

- ps_fo:

  Propensity-score formula.

- prin_fo:

  Principal-score formula.

- out_fo:

  Outcome-model formula.

- target_time:

  Non-empty numeric vector of observed standardized times. Baseline is
  allowed.

- B:

  Number of successful subject-level bootstrap replications.

- conf_level:

  Confidence level for Wald intervals based on bootstrap SDs.

- max_attempts:

  Maximum bootstrap attempts. `NULL` uses `10 * B`.

- verbose:

  Emit bootstrap progress messages.

## Value

A `pd_hte_timevarying` object containing time-specific estimates, the
explicitly requested `target_time`, and `bootstrap_info` with requested
and successful replicates, attempts, completion status, categorized
failures, and captured warnings.

## Details

The propensity, principal-score, and outcome models are refitted
internally for the point estimate and for every bootstrap sample.

## Examples

``` r
# \donttest{
data("BiSample", package = "PDRobust")
map <- Mapping(
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
#>   time covariate    estimate SD LowerBound UpperBound
#> 1    0 Intercept  0.08204782 NA         NA         NA
#> 2    0        X1  0.03944098 NA         NA         NA
#> 3    0        X2 -0.12088946 NA         NA         NA
#> 4    2 Intercept  0.02215715 NA         NA         NA
#> 5    2        X1 -0.07023692 NA         NA         NA
#> 6    2        X2  0.01739805 NA         NA         NA
# }
```

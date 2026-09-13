<div id="main" class="col-md-9" role="main">

# Estimate pooled heterogeneous treatment effects across all times

<div class="ref-description section level2">

`HTEAllT()` always uses every actual observed analysis time from the
mapped baseline through the mapped cutoff, inclusive. It does not accept
or use `target_time`. The propensity, principal-score, and outcome
models are refitted internally for the point estimate and for every
bootstrap sample. Within one analysis sample, a model fitted to the same
rows and formula is reused only to obtain the two counterfactual
treatment predictions.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

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

</div>

</div>

<div class="section level2">

## Arguments

-   data:

    A standardized `pd_data` object returned by `DataStandard()`.

-   ps\_fo:

    Propensity-score formula.

-   prin\_fo:

    Principal-score formula.

-   out\_fo:

    Outcome-model formula.

-   B:

    Number of successful subject-level bootstrap replications. Use `0`
    for point estimates only. Set a random seed before the call for
    reproducible resampling. Small values used in demonstrations are not
    sufficient for substantive interval estimation.

-   conf\_level:

    Confidence level for Wald intervals based on bootstrap SDs.

-   max\_attempts:

    Maximum bootstrap attempts. `NULL` uses `10 * B`.

-   verbose:

    Emit bootstrap progress messages.

-   progress\_callback:

    Optional function called with one named progress list before model
    fitting, after the point estimate, after every bootstrap attempt,
    and when bootstrap processing completes. The list contains `stage`,
    `successful`, `requested`, `attempts`, `max_attempts`,
    `failed_attempts`, `complete`, `elapsed_seconds`, and `updated_at`.
    Callback errors warn once and disable further updates without
    changing the analysis.

</div>

<div class="section level2">

## Value

A `pd_hte_pooled` object. `analysis_times` gives the complete
baseline-to-cutoff grid, `time_effect_estimable` records whether a time
effect was included, and `bootstrap_info` records requested and
successful replicates, attempts, completion status, categorized
failures, and captured warning counts, and model diagnostics. Numeric
estimates and interval summaries are rounded to three decimals only
after inference; `boot_mat` retains full precision.

</div>

<div class="section level2">

## Details

Repeated finite-prediction separation or convergence messages are
consolidated at the analysis boundary. Model-level details remain
available in `model_diagnostics`; bootstrap warnings and their counts
are stored in `bootstrap_info`.

If the prepared data contain only one analysis time, the estimator omits
the time-effect term and records that the time effect is not estimable.

</div>

<div class="section level2">

## Numerical safeguards

Propensity scores are clipped to `[0.01, 0.99]`, and their product with
treatment-1 survival probabilities is clipped to `[0.005, 0.995]` in the
estimating equation. These fixed limits stabilize denominators but
change the equation when active; they do not establish adequate overlap
or the causal assumptions. Inspect the model diagnostics and assess
sensitivity to sparse risk sets. Returned summaries are rounded only
after inference; full-precision bootstrap coefficients are in
`boot_mat`.

</div>

<div class="section level2">

## Treatment coding

The implemented estimator uses treatment `1` as the survival-favorable
arm: potential survival satisfies \\(S^1 \\ge S^0\\) at cutoff. Its
always-survivor principal score is therefore the survival probability
under treatment `0`. This convention is retained from version 0.3.7. The
main estimator in the reference below uses the opposite arm labels. To
analyze data coded in that convention, recode the raw treatment as
`1 - A` before mapping and standardizing. To report the original
contrast, negate the package estimate and transform an interval
`[lower, upper]` to `[-upper, -lower]`. `Mapping()` does not infer or
reverse treatment coding.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[PDRobust-package](https://whhuan.github.io/PD_Robust/reference/PDRobust-package.md),
[pd\_methods](https://whhuan.github.io/PD_Robust/reference/pd_methods.md)

</div>

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

</div>

</div>

</div>

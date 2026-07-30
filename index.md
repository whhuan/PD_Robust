# PDRobust

------------------------------------------------------------------------

editor_options: markdown: wrap: 80 —

PDRobust implements principal-stratification analyses for longitudinal
outcomes that may be truncated by death. Version 0.3.5 uses an explicit
mapping-driven workflow and refits each nuisance prediction model from
the data supplied to the current function call.

``` text
pd_example_data() -> Mapping() -> DataCheck() -> DataStandard()
                  -> prediction / diagnostic / analysis functions
```

## Installation

``` r

# install.packages("remotes")
# remotes::install_github("whhuan/PD_Robust")
```

## 1. Load example data with `pd_example_data()`

``` r

library(PDRobust)

binary_raw <- pd_example_data("binary")
continuous_raw <- pd_example_data("continuous")
imperfect_raw <- pd_example_data("imperfect")

class(binary_raw)       # "data.frame"
dim(binary_raw)
head(binary_raw)
```

`pd_example_data()` returns a long-format data frame. The bundled binary
data are used below for the main workflow; the continuous data are used
for sensitivity analysis with
[`SA()`](https://whhuan.github.io/PD_Robust/reference/SA.md).

## 2. Define roles and analysis settings with `Mapping()`

``` r

mapping <- Mapping(
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

class(mapping)          # "pd_mapping"
mapping
mapping$covariates
mapping$interest_vars
```

[`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md)
returns a `pd_mapping` object containing the structural column names,
raw baseline and cutoff times, all nuisance-model covariates, effect
modifiers, and the outcome type.

## 3. Validate the raw data with `DataCheck()`

``` r

check <- DataCheck(binary_raw, mapping)

class(check)                    # "pd_data_check"
check$ready_for_analysis
check$manual_resolution_required
check$checks                    # one row per validation rule
check$diagnostics               # affected rows, IDs, and time summaries
print(check)
```

[`DataCheck()`](https://whhuan.github.io/PD_Robust/reference/DataCheck.md)
never modifies the input data. It returns an itemized validation report
with pass/fail status, severity, analysis-blocking status, diagnostic
details, and recommended handling.

## 4. Standardize the panel with `DataStandard()`

``` r

pd_data <- DataStandard(binary_raw, mapping)

class(pd_data)                  # c("pd_data", "data.frame")
head(pd_data)
attr(pd_data, "pd_mapping")     # standardized mapping
attr(pd_data, "pd_original_mapping")
standardization <- attr(pd_data, "pd_standardization")
standardization$time_map
standardization$id_map
standardization$attrition
```

[`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md)
returns a sorted `pd_data` data frame. It safely converts explicit
binary encodings, maps IDs to consecutive integers, maps the observed
analysis-time grid to consecutive integers, and attaches mapping and
audit attributes. Use `drop = TRUE` only when the reported subject-level
exclusions are intended.

## Model formulas used below

``` r

ps_fo <- A ~ X1 + X2 + X4
prin_fo <- S ~ X1 + X2 + X4 + A + time
out_fo <- Y ~ X1 + X2 + A
standardized_mapping <- attr(pd_data, "pd_mapping")
```

## 5. Prediction functions

### `PSPred()`

``` r

ps <- PSPred(
  ps_fo,
  fit_dat = pd_data,
  pred_dat = pd_data,
  mapping = standardized_mapping
)

class(ps)               # c("pd_prediction", "numeric")
length(ps)              # nrow(pd_data)
head(ps)
```

[`PSPred()`](https://whhuan.github.io/PD_Robust/reference/PSPred.md)
fits a baseline logistic treatment model and returns one propensity
prediction for every row of `pred_dat`.

### `PrinPred()`

``` r

p0 <- PrinPred(
  prin_fo,
  fit_dat = pd_data,
  pred_dat = pd_data,
  treatment = 0,
  mapping = standardized_mapping
)

p1 <- PrinPred(
  prin_fo,
  fit_dat = pd_data,
  pred_dat = pd_data,
  treatment = 1,
  mapping = standardized_mapping
)

class(p0)               # c("pd_prediction", "numeric")
head(cbind(pd_data[c("id", "time")], p0, p1))
```

[`PrinPred()`](https://whhuan.github.io/PD_Robust/reference/PrinPred.md)
returns cumulative principal-survival probabilities under the requested
treatment. In longitudinal data it fits on post-baseline rows whose
subjects survived the immediately preceding observed time. In a
single-time analysis it uses all complete rows and does not construct an
at-risk indicator.

### `OutPred()`

``` r

outcome_fit_data <- pd_data[pd_data$S == 1, , drop = FALSE]

mu0 <- OutPred(
  out_fo,
  fit_dat = outcome_fit_data,
  pred_dat = pd_data,
  a = 0,
  mapping = standardized_mapping
)
mu1 <- OutPred(
  out_fo,
  fit_dat = outcome_fit_data,
  pred_dat = pd_data,
  a = 1,
  mapping = standardized_mapping
)

class(mu1)              # c("pd_prediction", "numeric")
head(cbind(mu0, mu1))
```

[`OutPred()`](https://whhuan.github.io/PD_Robust/reference/OutPred.md)
returns predicted outcomes after setting treatment to `a` and survival
to one in `pred_dat`. It uses linear regression for `y_type = "C"` and
logistic regression for `y_type = "B"`.

## 6. Diagnostics

### `PSDiag()`

``` r

ps_diagnostic <- PSDiag(pd_data, ps_fo)

class(ps_diagnostic)            # c("pd_exposure_diagnostic", "PSDiag")
ps_diagnostic$smd_before
ps_diagnostic$smd_after
summary(ps_diagnostic$weights)
range(ps_diagnostic$propensity) # always within 0.01 and 0.99
ps_diagnostic$plot
plot(ps_diagnostic)
```

[`PSDiag()`](https://whhuan.github.io/PD_Robust/reference/PSDiag.md)
returns unadjusted and IPTW-adjusted standardized mean differences,
ordinary IPTW weights, clipped propensity scores, plotting data, and a
ggplot. The propensity scores are always truncated internally by
`pmin(pmax(pi, 0.01), 0.99)` before weights are calculated.

### `PrinSDiag()`

``` r

principal_diagnostic <- PrinSDiag(pd_data, ps_fo, prin_fo)

class(principal_diagnostic)       # c("pd_principal_diagnostic", "PrinSDiag")
principal_diagnostic$statistics
range(principal_diagnostic$propensity) # always within 0.01 and 0.99
head(principal_diagnostic$p0)
head(principal_diagnostic$p1)
principal_diagnostic$plot
plot(principal_diagnostic)
```

[`PrinSDiag()`](https://whhuan.github.io/PD_Robust/reference/PrinSDiag.md)
returns cutoff-aligned standardized balance statistics, clipped
propensity scores, cumulative principal scores under treatment 0 and 1,
plotting data, and a ggplot.

## 7. Principal-stratum profiling with `QR()`

``` r

principal_profile <- QR(
  pd_data,
  prin_fo,
  quantile_level = c(0.25, 0.50, 0.75)
)

class(principal_profile)       # c("pd_principal_summary", "QR")
principal_profile$mean
principal_profile$quantile
head(principal_profile$weights)
print(principal_profile)
```

[`QR()`](https://whhuan.github.io/PD_Robust/reference/QR.md) returns
principal-score-weighted means and weighted quantiles of the mapped
interest variables at cutoff, together with the weights and mapping.

## 8. Treatment-group odds ratios with `ORCI()`

``` r

or_control <- ORCI(
  pd_data,
  S ~ X1 + X2 + X4,
  treatment_group = 0
)
or_treated <- ORCI(
  pd_data,
  S ~ X1 + X2 + X4,
  treatment_group = 1
)

class(or_control)              # c("pd_odds_ratios", "odds_ratios")
or_control$forestplotdat       # OR, lower bound, upper bound
or_control$model               # fitted cutoff logistic model
or_control$analysis_data
or_control$plot
plot(or_control)
```

[`ORCI()`](https://whhuan.github.io/PD_Robust/reference/ORCI.md) returns
treatment-group-specific cutoff odds ratios and confidence intervals,
the fitted logistic model, analysis data, settings, and a forest plot.

## 9. Time-specific HTEs with `HTESepT()`

``` r

separate_hte <- HTESepT(
  pd_data,
  ps_fo = ps_fo,
  prin_fo = prin_fo,
  out_fo = out_fo,
  target_time = c(1, 2),
  B = 0,
  verbose = FALSE
)

class(separate_hte)            # "pd_hte_timevarying"
separate_hte$summary           # time, term, estimate, SD, CI
separate_hte$target_time
separate_hte$convergence
separate_hte$bootstrap_info
separate_hte$boot_mat
separate_hte$forest_plot
plot(separate_hte)
```

[`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md)
returns an HTE estimate for each requested observed standardized time
and each mapped effect modifier, plus the intercept. `target_time`
controls reported outcome-analysis times only; principal scores still
accumulate over the full baseline-to-cutoff grid. Set `B > 0` for
subject-level bootstrap standard errors and Wald confidence intervals.

## 10. Pooled HTEs with `HTEAllT()`

``` r

pooled_hte <- HTEAllT(
  pd_data,
  ps_fo = ps_fo,
  prin_fo = prin_fo,
  out_fo = out_fo,
  B = 0,
  verbose = FALSE
)

class(pooled_hte)              # "pd_hte_pooled"
pooled_hte$summary             # term, estimate, SD, CI
pooled_hte$analysis_times
pooled_hte$time_effect_estimable
pooled_hte$convergence
pooled_hte$bootstrap_info
pooled_hte$boot_mat
pooled_hte$forest_plot
plot(pooled_hte)
```

[`HTEAllT()`](https://whhuan.github.io/PD_Robust/reference/HTEAllT.md)
always pools every observed standardized time from baseline through
cutoff. For a single analysis time it omits the time-effect term and
records that the time effect is not estimable.

## 11. Continuous-outcome sensitivity analysis with `SA()`

``` r

continuous_mapping <- Mapping(
  baseline_time = 0,
  cutoff_time = 2,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"),
  y_type = "C"
)
continuous_data <- DataStandard(continuous_raw, continuous_mapping)

set.seed(20260728)
sensitivity <- SA(
  continuous_data,
  A ~ X1 + X2 + X4,
  S ~ X1 + X2 + X4 + A + time,
  Y ~ X1 + X2 + A,
  ratiovec = c(0, 0.05, 0.10)
)

class(sensitivity)             # c("pd_sensitivity", "SA")
sensitivity$data               # tidy ratio-time-term estimates
sensitivity$beta_df_wide       # one row per ratio and time
sensitivity$variance_by_time
sensitivity$plot               # named list of ggplots
print(sensitivity)
```

[`SA()`](https://whhuan.github.io/PD_Robust/reference/SA.md) is
restricted to continuous outcomes. It returns HTE estimates under each
specified outcome-noise variance ratio, the observed outcome variance by
time, and one sensitivity plot per mapped interest variable.

## Returned object summary

| Function | Main returned class | Main components |
|----|----|----|
| `pd_example_data()` | `data.frame` | Long-format example data |
| [`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md) | `pd_mapping` | Structural roles, times, covariates, effect modifiers, outcome type |
| [`DataCheck()`](https://whhuan.github.io/PD_Robust/reference/DataCheck.md) | `pd_data_check` | Readiness flags, checks, diagnostics, settings |
| [`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md) | `pd_data` | Standardized panel plus mapping and audit attributes |
| [`PSPred()`](https://whhuan.github.io/PD_Robust/reference/PSPred.md) | `pd_prediction` | Row-aligned propensity predictions |
| [`PrinPred()`](https://whhuan.github.io/PD_Robust/reference/PrinPred.md) | `pd_prediction` | Row-aligned cumulative principal scores |
| [`OutPred()`](https://whhuan.github.io/PD_Robust/reference/OutPred.md) | `pd_prediction` | Row-aligned potential-outcome predictions |
| [`PSDiag()`](https://whhuan.github.io/PD_Robust/reference/PSDiag.md) | `PSDiag` | SMDs, IPTW weights, clipped propensity, plot |
| [`PrinSDiag()`](https://whhuan.github.io/PD_Robust/reference/PrinSDiag.md) | `PrinSDiag` | Standardized statistics, clipped propensity, `p0`, `p1`, plot |
| [`QR()`](https://whhuan.github.io/PD_Robust/reference/QR.md) | `QR` | Weighted means, weighted quantiles, weights |
| [`ORCI()`](https://whhuan.github.io/PD_Robust/reference/ORCI.md) | `odds_ratios` | Odds ratios, confidence intervals, model, plot |
| [`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md) | `pd_hte_timevarying` | Time-specific estimates, bootstrap results, plot |
| [`HTEAllT()`](https://whhuan.github.io/PD_Robust/reference/HTEAllT.md) | `pd_hte_pooled` | Pooled estimates, analysis times, bootstrap results, plot |
| [`SA()`](https://whhuan.github.io/PD_Robust/reference/SA.md) | `SA` | Sensitivity estimates, variance summaries, plots |

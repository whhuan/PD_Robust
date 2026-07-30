# Changelog

## PDRobust 0.3.6

### Estimation and bootstrap

- [`SA()`](https://whhuan.github.io/PD_Robust/reference/SA.md) now
  supports continuous and binary outcomes. Continuous analyses retain
  the original additive-noise and closed-form equations; binary analyses
  use logistic outcome prediction and the bounded-link HTE estimating
  equation.
- Subject-level bootstrap resampling still preserves complete panels and
  assigns a new bootstrap ID to every sampled cluster. Ordinary model
  warnings are now recorded without automatically rejecting otherwise
  finite, converged replicates.
- Bootstrap diagnostics now categorize rejected replicates and retain
  warnings emitted by accepted or rejected attempts. The arbitrary
  coefficient-magnitude rejection threshold was removed.
- Binary estimating equations use numerically stable logistic
  calculations and may accept a finite root reached at the iteration
  limit when its residual precision satisfies the requested tolerance.

### Prediction and validation

- [`OutPred()`](https://whhuan.github.io/PD_Robust/reference/OutPred.md)
  retains the original missing-outcome filtering, treatment and survival
  assignments, linear/logistic model choice, response prediction, and
  row-aligned numeric return value.
- Separation, extreme fitted probabilities, rank-deficient nuisance
  fits, and ordinary fitting or prediction warnings are no longer fatal
  when finite predictions remain available. Genuinely non-finite or
  misaligned predictions and non-estimable HTE modifier systems remain
  errors.
- Ill-conditioned but full-rank closed-form estimating systems now warn
  and are accepted only when solving produces finite coefficients.

### Data, documentation, plots, and tests

- Examples now use the package datasets `BiSample` and
  `ImperfectConSample` through standard
  [`data()`](https://rdrr.io/r/utils/data.html) loading. The redundant
  CSV-backed `pd_example_data()` helper was removed.
- Pooled HTE and ORCI forest plots use stable, distinct variable colors
  with matching point, interval, and legend mappings.
- Tests now cover binary and continuous sensitivity analysis, finite
  warning-tolerant nuisance prediction, successful built-in-data
  bootstrap estimation, categorized bootstrap diagnostics, and plot
  color mappings.

## PDRobust 0.3.5

### Model validation

- Propensity-score, principal-score, outcome, odds-ratio,
  quantile-regression, HTE, and sensitivity-analysis fitting now use
  shared package-level preflight checks.
- Missing formula variables, invalid model matrices, zero-variance
  predictors, rank deficiency, insufficient complete cases, nonvarying
  responses, separation, non-estimable coefficients, fitting warnings,
  convergence failures, and singular estimating systems now produce
  contextual PDRobust errors instead of leaking raw model-fitting
  conditions.

### Tests

- Added a deterministic, side-effect-free simulation helper adapted from
  the package’s example-data generator. It creates continuous, binary,
  valid, and deliberately invalid test panels.
- Expanded data workflow tests for validation contracts, supported and
  unsupported encodings, edge cases, immutability, reproducibility,
  audit attributes, attrition, and value idempotency.
- Expanded prediction, analysis, profile, sensitivity, and diagnostic
  tests for boundary inputs, model-matrix validity, estimability,
  separation, convergence, reproducibility, and preservation of user
  data.

## PDRobust 0.3.4

### Diagnostics

- [`PSDiag()`](https://whhuan.github.io/PD_Robust/reference/PSDiag.md)
  now always truncates internally estimated propensity scores to
  `[0.01, 0.99]` before ordinary IPTW weights and weighted SMDs are
  calculated.
- [`PrinSDiag()`](https://whhuan.github.io/PD_Robust/reference/PrinSDiag.md)
  now applies the same fixed propensity-score truncation before
  evaluating the cutoff principal-score diagnostic equation.

### Tests

- Principal-score prediction fixtures now use a larger probabilistically
  generated panel with non-separated survival outcomes and a full-rank
  design matrix.
- Tests explicitly verify principal-model rank, response variation,
  convergence, absence of fitting warnings, and fixed diagnostic
  propensity truncation.

### Documentation

- The README now demonstrates the complete public workflow and
  identifies the principal returned class and components of every
  exported function.
- All vignettes were revised to document data preparation, independent
  prediction models, diagnostics, profiling, HTE estimation, and
  sensitivity analysis under the 0.3.4 interface.

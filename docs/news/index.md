<div id="main" class="col-md-9" role="main">

# Changelog

<div class="section level2">

## PDRobust 0.3.8

<div class="section level3">

### Data validation and standardization

-   Replaced repeated subject-level lookups with grouped visit counts,
    treatment flags, and stable survival ordering. Both validation
    passes and the existing public interfaces, data ordering, and audit
    attributes remain.
-   Documented all `DataCheck()` return fields and `DataStandard()`
    attributes, including the distinction between repairability and
    final analysis readiness. Corrected the data-workflow vignette’s
    endpoint, row-removal, and metadata descriptions to match the
    existing implementation.
-   Added regression coverage for factor-covariate attrition and the
    final readiness check when deletion removes a treatment group.
-   Excluded the large-sample development script and saved test plot
    from source archives; these files remain in the working repository.

</div>

<div class="section level3">

### CRAN release preparation

-   Retained the 0.3.7 treatment-1 survival-favorable estimator and
    documented how to convert treatment coding and effect contrasts
    relative to Zhang et al. (2026). Added the methodological citation
    and an implementation guide. Outcome-noise sensitivity is explicitly
    distinguished from the paper’s principal-ignorability sensitivity
    procedure.
-   Corrected the documented `ORCI()` argument spelling and QR return
    values without changing function signatures or numerical estimators.
-   Documented all bundled data columns and the existing print, plot,
    and subsetting methods.
-   Added reproducible seeds and explained demonstration-only bootstrap
    counts in the README and workflow vignettes; clarified the binary
    effect link and the interpretation of outcome-noise sensitivity
    analysis.
-   Declared the utility namespace import used for global-variable
    registration and excluded development reports, release artifacts,
    and the top-level sensitivity image from source builds.
-   Shortened the package title and made the software citation follow
    the package metadata.
-   Corrected vignette figure paths for the documentation website and
    supplied descriptive alternative text for workflow figures.

</div>

</div>

<div class="section level2">

## PDRobust 0.3.7.2

<div class="section level3">

### Live bootstrap progress

-   `HTEAllT()` and `HTESepT()` accept an optional `progress_callback`
    without changing their estimands, fitting logic, bootstrap
    acceptance rules, or returned numerical results.
-   The callback receives structured updates before model fitting, after
    the point estimate, after every bootstrap attempt, and at
    completion. Updates report successful replications, total and failed
    attempts, elapsed time, and the last worker-update time.
-   A callback error produces one warning and disables monitoring for
    that run; it does not interrupt or alter the scientific calculation.

</div>

</div>

<div class="section level2">

## PDRobust 0.3.7

<div class="section level3">

### Interfaces and example data

-   All ten `Mapping()` arguments are required and every package example
    now supplies the five structural column roles explicitly.
-   `ORCI()` now requires the treatment-group argument `a`; the obsolete
    `treatment_group` argument and its default were removed.
-   Tests and vignettes now use the current `ImperfectConSample`
    contract: noncanonical clinical column names, character visit months
    0/6/12, preserved X1-X6 covariate names, and explicitly reported
    recoverable imperfections.

</div>

<div class="section level3">

### Model warnings and diagnostics

-   Logistic warnings are normalized so nonconvergence and separation
    are each reported once per fit instead of duplicating both `glm()`
    and package-level messages.
-   `HTESepT()`, `HTEAllT()`, and `SA()` consolidate repeated
    point-estimate nuisance warnings at the public analysis boundary.
    Bootstrap warnings remain silent during resampling and are
    aggregated in `bootstrap_info`.
-   Analysis-internal principal-score and outcome models are fitted once
    for each distinct data/formula combination and reused for the two
    counterfactual treatment predictions, eliminating identical
    duplicate fits without changing the prediction equations.
-   Returned model diagnostics identify the analysis, sample type,
    target time, treatment group, fitting rows and subjects, response
    counts, formula, predictors, rank status, finite-prediction status,
    convergence, and separation.

</div>

<div class="section level3">

### Return precision

-   Final user-facing predictions, estimates, diagnostics, confidence
    intervals, odds ratios, weighted summaries, and sensitivity tables
    are rounded to three decimal places.
-   Full precision is retained for analysis data, internal nuisance
    predictions, fitted models, probabilities, weights, score equations,
    optimization, bootstrap replicates, and confidence-interval
    calculations.
-   `generate_data_example()` performs its simulation at full precision
    and rounds only the final generated data frame.

</div>

<div class="section level3">

### Validation cleanup

-   Binary conversion and invalid-row detection now share one
    authoritative implementation.
-   Validation guaranteed by `Mapping()` is no longer repeated by
    `DataCheck()`.
-   `DataStandard()` now consumes the authoritative initial
    `DataCheck()` result instead of repeating mapping, column, and
    nonempty-data checks.
-   Prepared-data helpers now reuse the validated mapping instead of
    retrieving and validating it multiple times in the same public call.

</div>

</div>

<div class="section level2">

## PDRobust 0.3.6

<div class="section level3">

### Estimation and bootstrap

-   `SA()` now supports continuous and binary outcomes. Continuous
    analyses retain the original additive-noise and closed-form
    equations; binary analyses use logistic outcome prediction and the
    bounded-link HTE estimating equation.
-   Subject-level bootstrap resampling still preserves complete panels
    and assigns a new bootstrap ID to every sampled cluster. Ordinary
    model warnings are now recorded without automatically rejecting
    otherwise finite, converged replicates.
-   Bootstrap diagnostics now categorize rejected replicates and retain
    warnings emitted by accepted or rejected attempts. The arbitrary
    coefficient-magnitude rejection threshold was removed.
-   Binary estimating equations use numerically stable logistic
    calculations and may accept a finite root reached at the iteration
    limit when its residual precision satisfies the requested tolerance.

</div>

<div class="section level3">

### Prediction and validation

-   `OutPred()` retains the original missing-outcome filtering,
    treatment and survival assignments, linear/logistic model choice,
    response prediction, and row-aligned numeric return value.
-   Separation, extreme fitted probabilities, rank-deficient nuisance
    fits, and ordinary fitting or prediction warnings are no longer
    fatal when finite predictions remain available. Genuinely non-finite
    or misaligned predictions and non-estimable HTE modifier systems
    remain errors.
-   Ill-conditioned but full-rank closed-form estimating systems now
    warn and are accepted only when solving produces finite
    coefficients.

</div>

<div class="section level3">

### Data, documentation, plots, and tests

-   Examples now use the package datasets `BiSample` and
    `ImperfectConSample` through standard `data()` loading. The
    redundant CSV-backed `pd_example_data()` helper was removed.
-   Pooled HTE and ORCI forest plots use stable, distinct variable
    colors with matching point, interval, and legend mappings.
-   Tests now cover binary and continuous sensitivity analysis, finite
    warning-tolerant nuisance prediction, successful built-in-data
    bootstrap estimation, categorized bootstrap diagnostics, and plot
    color mappings.

</div>

</div>

<div class="section level2">

## PDRobust 0.3.5

<div class="section level3">

### Model validation

-   Propensity-score, principal-score, outcome, odds-ratio,
    quantile-regression, HTE, and sensitivity-analysis fitting now use
    shared package-level preflight checks.
-   Missing formula variables, invalid model matrices, zero-variance
    predictors, rank deficiency, insufficient complete cases, nonvarying
    responses, separation, non-estimable coefficients, fitting warnings,
    convergence failures, and singular estimating systems now produce
    contextual PDRobust errors instead of leaking raw model-fitting
    conditions.

</div>

<div class="section level3">

### Tests

-   Added a deterministic, side-effect-free simulation helper adapted
    from the package’s example-data generator. It creates continuous,
    binary, valid, and deliberately invalid test panels.
-   Expanded data workflow tests for validation contracts, supported and
    unsupported encodings, edge cases, immutability, reproducibility,
    audit attributes, attrition, and value idempotency.
-   Expanded prediction, analysis, profile, sensitivity, and diagnostic
    tests for boundary inputs, model-matrix validity, estimability,
    separation, convergence, reproducibility, and preservation of user
    data.

</div>

</div>

<div class="section level2">

## PDRobust 0.3.4

<div class="section level3">

### Diagnostics

-   `PSDiag()` now always truncates internally estimated propensity
    scores to `[0.01, 0.99]` before ordinary IPTW weights and weighted
    SMDs are calculated.
-   `PrinSDiag()` now applies the same fixed propensity-score truncation
    before evaluating the cutoff principal-score diagnostic equation.

</div>

<div class="section level3">

### Tests

-   Principal-score prediction fixtures now use a larger
    probabilistically generated panel with non-separated survival
    outcomes and a full-rank design matrix.
-   Tests explicitly verify principal-model rank, response variation,
    convergence, absence of fitting warnings, and fixed diagnostic
    propensity truncation.

</div>

<div class="section level3">

### Documentation

-   The README now demonstrates the complete public workflow and
    identifies the principal returned class and components of every
    exported function.
-   All vignettes were revised to document data preparation, independent
    prediction models, diagnostics, profiling, HTE estimation, and
    sensitivity analysis under the 0.3.4 interface.

</div>

</div>

</div>

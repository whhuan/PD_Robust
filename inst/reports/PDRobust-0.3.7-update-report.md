# PDRobust 0.3.7 update report

## Scope and baseline

The authoritative implementation baseline was `0.3.6.zip`. The `0.0` and
`0.1` archives were consulted only to compare historical nuisance-model
subsets, fitting paths, repeated fits, and warning handling. No restored 0.3.6
algorithm was replaced by an older implementation.

## Public interface changes

### `Mapping()`

The authoritative 0.3.6 source already declared all ten arguments without
defaults, although package examples and tests still called it as if the five
structural roles had defaults.

```r
# 0.3.6 source signature
Mapping <- function(id, time, treatment, survival, outcome,
                    baseline_time, cutoff_time,
                    covariates, interest_vars, y_type)

# 0.3.7 signature
Mapping <- function(id, time, treatment, survival, outcome,
                    baseline_time, cutoff_time,
                    covariates, interest_vars, y_type)
```

Version 0.3.7 makes the existing required interface consistent package-wide:
all executable calls, examples, tests, README code, vignettes, and generated
help explicitly supply all structural roles. A regression test verifies the
exact argument order and confirms that every formal is missing by default.
There is no wrapper or internal fallback that restores a hidden default.

### `ORCI()`

```r
# 0.3.6
ORCI <- function(data, fomula, treatment_group = 0,
                 conf_level = 0.95)

# 0.3.7
ORCI <- function(data, fomula, a, conf_level = 0.95)
```

`a` is required and accepts only `0` or `1`. The obsolete
`treatment_group` formal, default, forwarding, messages, examples, and
documentation were removed. Diagnostic field names describing treatment-group
counts remain because they describe data, not the removed API.

No other exported function signature changed.

## Changes grouped by the six requested tasks

### 1. Required `Mapping()` arguments

- Preserved the exact ten-argument, no-default implementation.
- Updated every package call to supply `id`, `time`, `treatment`, `survival`,
  and `outcome` explicitly.
- Added tests for exact formals, standard missing-argument errors, standard and
  custom column names, and stored analysis settings.

### 2. Current `ImperfectConSample` contract

The authoritative generated object has 587 rows and these 11 columns:

```text
patient_id, visit_month, alive_status, treatment, clinical_outcome,
X1, X2, X3, X4, X5, X6
```

- Raw visits are character values `0`, `6`, and `12`; baseline and cutoff are
  therefore mapped as 0 and 12.
- `patient_id`, `treatment`, and `alive_status` are deliberately noncanonical
  character encodings.
- Recoverable imperfections include an unidentified row, an incomplete panel,
  a missing X1 value, a missing outcome among survivors, unsorted records, and
  structural outcome missingness after death.
- X1-X6 are intentionally retained as the covariates.
- `DataCheck()` reports that the object can be standardized but is not yet
  analysis-ready.
- `DataStandard(..., drop = TRUE)` produces 576 rows for 192 subjects, maps
  visits to integer times 0:2, removes four affected subjects, preserves the
  custom column names, and returns an analysis-ready `pd_data` object.
- Tests now verify stable contract properties and the mapping-driven workflow
  instead of obsolete canonical column names or brittle generated values.
- The dataset structure was not changed to satisfy tests. The two bundled data
  objects were changed only as required by the final-output precision policy.

### 3. Required `ORCI(a)`

- Renamed `treatment_group` to required `a` throughout implementation,
  validation, returned settings, documentation, examples, vignettes, and tests.
- Added tests for a missing `a`, values 0 and 1, invalid values, group-specific
  fitting, and rejection of the obsolete named argument.

### 4. Logistic warnings and model diagnostics

#### Findings

The warnings are statistically genuine when they occur:

- `PrinPred` fits a conditional survival model on post-baseline rows that were
  alive at the immediately preceding observed visit. Survival attrition,
  treatment imbalance, sparse later risk sets, and principal-stratum sparsity
  can leave a nearly separated binary response or a nonconvergent logistic fit.
- `OutPred` inside `HTESepT` fits on the requested time-specific rows belonging
  to subjects who survive at cutoff. For a binary outcome, low event rates plus
  this smaller effective subgroup can produce complete or quasi-complete
  separation. `HTEAllT` uses the longitudinal rows for cutoff survivors, which
  can still be sparse or imbalanced.
- Direct nuisance-model calls need not reproduce an analysis warning when they
  use a larger or different fitting subset.
- Bootstrap resampling can remove response classes or make an already sparse
  subgroup more extreme even when the original sample is usable.
- The 0.3.6 analysis path fitted an identical principal model once per
  counterfactual arm and an identical outcome model once per arm at each
  analysis time. Those duplicated fits repeated the same warning.
- The current multi-time principal-score contract intentionally excludes
  baseline rows from the conditional-survival risk set. Historical baseline
  inclusion was examined and was not restored.

#### Historical comparison

- Version 0.0 used direct `glm()`/`lm()` calls and did not have the current
  separation heuristic, model preflight, normalized warning, or returned
  diagnostics. A genuine raw `glm` warning could appear, but no package-level
  classification made the instability visible in a structured result.
- The active version 0.1 principal path used `speedglm.wfit`, while its outcome
  path called `glm()` or `lm()` directly. Its HTE functions fitted the
  principal and outcome models separately for both treatment predictions.
  Bootstrap warning handlers discarded a warned attempt without retaining
  model context. A newer validation file bundled in that archive was not used
  by those active HTE paths.
- Consequently, the reported absence of warnings in the older versions does
  not demonstrate more stable data or a statistically superior fit; the
  warnings were not classified or retained in the same way.

#### Version 0.3.7 policy

- Raw `glm.fit` nonconvergence and extreme-probability warnings are captured at
  the individual fit, classified, and replaced by one normalized package
  warning. Nonconvergence and separation remain distinct diagnostic flags.
- Finite prediction-based fits are retained when the estimator does not
  interpret unstable coefficients. Missing, non-finite, or misaligned
  predictions remain errors.
- `HTESepT()`, `HTEAllT()`, and `SA()` emit one consolidated warning at the
  public analysis boundary rather than one message per arm, time, or scenario.
  Unrelated warnings are not suppressed.
- Within one analysis sample, one principal fit and one outcome fit for a given
  formula/data subset are reused for both counterfactual predictions. Numerical
  comparisons confirm that the predictions and estimating equations are
  unchanged.
- Bootstrap warnings do not flood the console. Attempt-level warnings,
  aggregated warning counts, categorized failures, and warned-model
  diagnostics are returned in `bootstrap_info`.
- Original-sample diagnostics report analysis, sample, attempt, target time,
  treatment (or `NA` for a shared counterfactual fit), fitting rows, subjects,
  response counts, formula, predictors, design rank status, prediction
  finiteness, convergence, separation, and warning text.
- No treatment balancing, equal group sizing, event-rate inflation, survival
  weakening, formula change, or risk-set change was used to silence warnings.
  Data generation changed only at its final rounding boundary.

### 5. Three-decimal public outputs with full-precision computation

- Added centralized helpers used only while constructing public return values.
- `PSPred()`, `PrinPred()`, and `OutPred()` now wrap full-precision internal
  implementations. HTE, diagnostics, QR, and SA call the internal
  implementations so public prediction rounding cannot enter an estimating
  equation.
- HTE point estimates, standard errors, and confidence limits are rounded only
  after bootstrap inference. `boot_mat`, convergence details, nuisance
  predictions, model fits, probabilities, weights, score equations, and solver
  inputs retain full precision.
- ORCI odds-ratio tables; HTE summaries; diagnostic statistics; QR means and
  quantiles; SA estimates and displayed variances; and display-only data-check
  percentages are rounded to three decimals.
- Analysis data, mapping/configuration values, fitted models, returned
  probabilities and weights needed for diagnostics, identifiers, counts,
  integer times, and logical flags retain their values and storage modes.
- `generate_data_example()` simulates entirely at full precision and rounds
  double-valued output columns only after the final data frame has been
  assembled. Integer and character columns are not coerced.
- The bundled generated `.rda` objects were regenerated at the same final
  boundary and satisfy the three-decimal/type contract.

### 6. Validation and code cleanup

#### Removed or consolidated

- `.pd_binary_conversion()` now returns invalid row indices, replacing the
  duplicate `.pd_binary_invalid_rows()` implementation.
- Removed the unreachable `interest_vars_in_covariates` data check because
  `Mapping()` rejects such a mapping at construction.
- Removed unused `required_times <- NULL` assignments.
- `DataStandard()` now performs one authoritative `.pd_check_data_impl()` call
  and reuses its validated mapping and report instead of repeating mapping,
  nonempty-data, and required-column checks.
- Prepared-data helpers return and reuse the validated mapping, avoiding a
  second retrieval and validation in ORCI, PSDiag, PrinSDiag, and QR.
- Removed ORCI's duplicate response-level check; the checked logistic fitting
  path owns that validation.
- Removed identical analysis-internal nuisance refits while preserving
  independent fitting for each public predictor call.
- Removed unreferenced `inst/extdata` CSV copies whose old intermediate schemas
  disagreed with the authoritative `.rda` datasets.
- Removed embedded repository/editor state, histories, macOS metadata, cached
  README figures, a knitted vignette HTML file, and generated pkgdown output.

#### Retained

The package continues to validate mapping structure and columns, finite
baseline/cutoff times, duplicate ID-time rows, binary encodings, missingness and
attrition, complete panels, survival monotonicity, structural outcomes,
formula/design validity, response variation, finite predictions, estimating
system rank/conditioning, and numerical convergence. These checks prevent
undefined or misleading estimates and are not migration-only checks.

The principal risk-set construction, outcome subset definitions, structural
outcome handling, solver checks, and statistically meaningful model
instability checks were left unchanged where historical comparison did not
justify removal.

## Modified and added package files

### Metadata and top-level documentation

- `DESCRIPTION`
- `NEWS.md`
- `README.Rmd`
- `README.md`

### R source

- `R/DataCheck.R`
- `R/DataStandard.R`
- `R/HTEAllT.R`
- `R/HTESepT.R`
- `R/Mapping.R`
- `R/ORCI.R`
- `R/OutPred.R`
- `R/PSDiag.R`
- `R/PSPred.R`
- `R/PrinPred.R`
- `R/PrinSDiag.R`
- `R/QR.R`
- `R/SA.R`
- `R/data_validation.R`
- `R/hte_common.R`
- `R/model-validation.R`
- `R/public-helpers.R`
- `R/utils.R`

### Generated data

- `data-raw/generate_example_data.R`
- `data/BiSample.rda`
- `data/ImperfectConSample.rda`

### Generated help, regenerated from the corresponding roxygen comments

- `man/DataCheck.Rd`
- `man/DataStandard.Rd`
- `man/HTEAllT.Rd`
- `man/HTESepT.Rd`
- `man/Mapping.Rd`
- `man/ORCI.Rd`
- `man/OutPred.Rd`
- `man/PSDiag.Rd`
- `man/PSPred.Rd`
- `man/PrinPred.Rd`
- `man/PrinSDiag.Rd`
- `man/QR.Rd`
- `man/SA.Rd`

`NAMESPACE` did not require a change because the export and S3 registrations
are unchanged; all registered symbols were nevertheless resolved during
validation.

### Test helpers and tests

- `tests/testthat/helper-data.R`
- `tests/testthat/helper-reference.R`
- `tests/testthat/helper-simulation.R`
- `tests/testthat/test-cran-rjournal-contracts.R`
- `tests/testthat/test-custom-mapping.R`
- `tests/testthat/test-data-validation-expanded.R`
- `tests/testthat/test-data-workflow.R`
- `tests/testthat/test-diagnostics.R`
- `tests/testthat/test-edge-cases.R`
- `tests/testthat/test-errors-validation.R`
- `tests/testthat/test-example-data.R`
- `tests/testthat/test-hte-bootstrap.R`
- `tests/testthat/test-interface.R`
- `tests/testthat/test-model-preflight-035.R`
- `tests/testthat/test-plot-colors.R`
- `tests/testthat/test-predictions.R`
- `tests/testthat/test-restored-algorithms.R`
- `tests/testthat/test-return-objects.R`
- `tests/testthat/test-row-order-alignment.R`
- `tests/testthat/test-time-scope.R`
- `tests/testthat/test-return-rounding-037.R` (new)
- `tests/testthat/test-warning-policy-037.R` (new)

### Vignettes

- `vignettes/Functions.Rmd`
- `vignettes/data-requirements.Rmd`
- `vignettes/diagnostics-and-sensitivity.Rmd`
- `vignettes/getting-started.Rmd`
- `vignettes/prediction-models.Rmd`

### Report

- `inst/reports/PDRobust-0.3.7-update-report.md` (new)

## Removed files and generated/cached trees

- Embedded `.git/` repository metadata.
- `.Rproj.user/` editor state.
- `docs/` generated pkgdown site.
- `README_files/` generated README figures.
- `vignettes/Functions.html` generated vignette.
- `man/figures/README-unnamed-chunk-13-1.png` stale generated figure.
- `inst/extdata/BiSample.csv` and
  `inst/extdata/ImperfectConSample.csv` obsolete dataset copies.
- All package-contained `.DS_Store`, `.Rhistory`, and `.Rapp.history` files.

The authored package sources, tests, workflows, license files, project file,
dataset documentation, current `.rda` data, and referenced
`man/figures/fcfigure.png` remain in the complete package.

## Test and verification results

### Executed targeted validation

The available lightweight R 4.6.0 WebAssembly runtime was used for syntax and
base/stats execution. All 26 named checks passed:

1. All 48 R source, generator, helper, and test files parsed.
2. All 16 Rd help topics parsed.
3. DESCRIPTION version is 0.3.7.
4. All NAMESPACE exports resolved.
5. All NAMESPACE S3 methods resolved.
6. `Mapping()` has the exact required interface.
7. `ORCI()` has the required `a` interface.
8. The 27 executable `Mapping()` calls supply all roles except the one
   intentional missing-`id` error test.
9. An omitted `Mapping()` argument raises the standard R error.
10. `ImperfectConSample` matches its raw contract.
11. Bundled generated doubles have at most three decimals.
12. Bundled structural storage modes are preserved.
13. The imperfect-data validation contract passes.
14. The imperfect-data standardization contract passes.
15. Public predictors equal rounded full-precision predictions.
16. Predictor internals retain additional precision.
17. Paired internal predictions equal independent full-precision predictions.
18. Normal HTE and SA analyses do not warn.
19. Duplicate internal nuisance fits are absent.
20. Public HTE summaries equal rounded full-precision estimates.
21. Bootstrap inference is performed before rounding, and `boot_mat` retains
    additional precision.
22. Deterministic quasi-separation is retained and diagnosed once.
23. Deterministic nonconvergence is distinct and diagnosed once.
24. Repeated analysis warnings are consolidated.
25. Bootstrap warnings aggregate by message.
26. The generator has no rounding before its final dataset boundary.

A separate balanced-parenthesis text audit covered code, roxygen examples,
generated help, README code, and vignettes: 63 complete `Mapping()` calls, one
intentional missing-argument test, and zero invalid calls.

The deterministic bootstrap check requested three successful replicates and
obtained all three in three attempts, with no warning in that normal-data
scenario.

### Full package tooling not available

This environment did not contain a native R executable or the package
dependencies `testthat`, `roxygen2`, `ggplot2`, `quantreg`, and `rootSolve`.
Network policy also prevented installing the WebAssembly package builds.
Therefore these commands could not be executed:

```r
devtools::document()
devtools::test()
testthat::test_dir("tests/testthat")
devtools::check()
```

Likewise, native `R CMD check` was not available. Consequently there is no
honest native-check ERROR/WARNING/NOTE count to report; the status is **not
run**, not an asserted `0/0/0`. Updated public `.Rd` files were generated
deterministically from the roxygen source blocks and all help files parse, but
the result should still receive a native `devtools::document()` and
`R CMD check` in a standard R 4.4.3 development environment before release.

Runtime checks requiring the unavailable `ggplot2`, `quantreg`, or `rootSolve`
dependencies were not executed in the lightweight runtime. Their source,
examples, registrations, and tests parse, and targeted base/stats estimator
paths passed, but this dependency limitation remains the principal unresolved
verification item.

### Archive integrity

The deliverable contains one top-level `PDRobust/` source directory and 88
regular package files. ZIP CRC testing passed for every entry. The archive
contains no embedded `.git/`, `.Rproj.user/`, generated pkgdown site, HTML
build output, history file, R workspace, or macOS metadata.

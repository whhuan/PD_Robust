## Draft submission notes: PDRobust 0.3.8

This is a proposed first CRAN submission. These notes are a preparation draft:
current R-devel and current-release Windows/macOS checks remain pending and
must be recorded before submission.

These results apply to the fresh archive built after the `ORCI()` argument-name
correction, print-method review, related documentation updates, and regenerated
vignettes. They supersede the earlier local 0.3.8 check results.

## Completed test environment

- Local macOS Sequoia 15.7.9, x86_64, R 4.4.3 (2025-02-28).
- Source archive built with all seven vignettes and checked with `--as-cran`.
- The full archive test suite passed. The source-checkout suite also passed
  after adding regression coverage for the corrected named argument and the
  display behavior of every package-specific print method.

## Check results

The final network-enabled check reported 0 errors, 0 warnings, and 3 notes:

- CRAN incoming feasibility: "New submission".
- Future file timestamps: "unable to verify current time" on the checking host.
- HTML math-rendering verification was skipped because the optional V8 package
  was unavailable on the local checking host.

All seven vignettes rebuilt and the HTML and PDF manuals both passed. The
network-enabled check also validated the package URLs. No package-caused errors,
warnings, or actionable notes remain in this local check.

## Interface and printed results

The public `ORCI()` argument is now spelled `formula` consistently in the
function, validation messages, help, examples, vignettes, README, website, and
tests. Positional calls are unchanged; calls using the former misspelling by
name must be updated.

Every package-specific print method introduces its result and displays the
principal numeric or tabular component. Objects containing user-facing plots
also display the stored plot or plots. This changes display behavior only; the
statistical calculations and plot objects are unchanged.

## Data-preparation review

The grouped DataCheck/DataStandard implementation preserves both validation
passes, public signatures, return fields, row ordering, and audit attributes.
Fresh old/new comparisons passed for 2,008 object/condition comparisons and
ten full-precision prediction/HTE comparisons. Additional regression tests
cover factor-covariate attrition and final readiness after a treatment group
is removed. Documentation clarifies existing behavior; no production R
expressions in the DataCheck/DataStandard implementation were changed in this
review.

## Method and documentation

The package retains its existing treatment convention: treatment 1 is the
survival-favorable arm. Documentation explains the conversion from the
opposite labels in Zhang et al. (2026), arXiv:2608.06654, including the effect
and confidence-interval sign changes. The estimating equations are unchanged.
The implemented sensitivity function is documented as outcome-noise
sensitivity, distinct from the paper's principal-ignorability sensitivity.

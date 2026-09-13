## Draft submission notes: PDRobust 0.3.8

This is a proposed first CRAN submission. These notes are a preparation draft:
current R-devel and current-release Windows/macOS checks remain pending and
must be recorded before submission.

These results apply to the fresh archive built after reviewing the changes in
commit 948ecc0 and applying the documented help, vignette, test, and archive
exclusion updates. They supersede the earlier local 0.3.8 check results.

## Completed test environment

- Local macOS Sequoia 15.7.9, x86_64, R 4.4.3 (2025-02-28).
- Source archive built with all seven vignettes and checked with `--as-cran`.
- Archive tests: 540 passes, 0 failures, 0 warnings, 1 skip. The skipped
  development-generator test requires `data-raw`, which is intentionally
  excluded from the package archive. The fresh source-checkout test suite,
  including the generator test, passed without skips.

## Check results

The final check reported 0 errors, 0 warnings, and 2 notes:

- CRAN incoming feasibility: "New submission".
- Future file timestamps: "unable to verify current time" on the checking host.

All seven vignettes rebuilt and the HTML and PDF manuals both passed, including
mathematical rendering with the optional V8 checker available. No package-caused
errors, warnings, or actionable notes remain in this local check.

## Data-preparation review

The grouped DataCheck/DataStandard implementation preserves both validation
passes, public signatures, return fields, row ordering, and audit attributes.
Fresh old/new comparisons passed for 2,008 object/condition comparisons and
ten full-precision prediction/HTE comparisons. Additional regression tests
cover factor-covariate attrition and final readiness after a treatment group
is removed. Documentation clarifies existing behavior; no production R
expressions were changed in this review.

## Method and documentation

The package retains its existing treatment convention: treatment 1 is the
survival-favorable arm. Documentation explains the conversion from the
opposite labels in Zhang et al. (2026), arXiv:2608.06654, including the effect
and confidence-interval sign changes. The estimating equations are unchanged.
The implemented sensitivity function is documented as outcome-noise
sensitivity, distinct from the paper's principal-ignorability sensitivity.

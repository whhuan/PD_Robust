## Draft submission notes: PDRobust 0.3.8

This is a proposed first CRAN submission. These notes are a preparation draft:
current R-devel and current-release Windows/macOS checks remain pending and
must be recorded before submission.

## Completed test environment

- Local macOS Sequoia 15.7.9, x86_64, R 4.4.3 (2025-02-28).
- Source archive built with all seven vignettes and checked with `--as-cran`.
- Existing tests: 512 passes, 0 failures, 0 warnings, 1 skip. The skipped
  development-generator test requires `data-raw`, which is intentionally
  excluded from the package archive; both shipped datasets were separately
  regenerated and compared successfully in the source checkout.

## Check results

The final check reported 0 errors, 0 warnings, and 2 notes:

- CRAN incoming feasibility: "New submission".
- Future file timestamps: "unable to verify current time" on the checking host.

The HTML and PDF manuals both passed. An initial environment-only note about
skipped mathematical rendering was resolved by installing the optional `V8`
checker in an isolated library and repeating the check on the unchanged archive.

## Method and documentation

The package retains its existing treatment convention: treatment 1 is the
survival-favorable arm. Documentation explains the conversion from the
opposite labels in Zhang et al. (2026), arXiv:2608.06654, including the effect
and confidence-interval sign changes. The estimating equations are unchanged.
The implemented sensitivity function is documented as outcome-noise
sensitivity, distinct from the paper's principal-ignorability sensitivity.

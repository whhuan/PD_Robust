# Documentation language revision

Date: 2026-09-21

## Purpose

This revision makes the package documentation easier for users to read while
preserving the implemented methods and interfaces. Roxygen comments in `R/`
remain the documentation source, and the corresponding files in `man/` were
regenerated with roxygen2.

No function arguments, statistical calculations, return structures, examples,
or executable code were changed.

## Main language changes

- Replaced developer-oriented phrases such as "structural columns," "analysis
  grid," "nuisance probabilities," "estimating diagnostics," and "categorized
  failures" with descriptions of what users provide, what the functions do,
  and what the returned information means.
- Explained model fitting and prediction in terms of treatment probabilities,
  cumulative survival probabilities, and predicted outcome means.
- Reworded return-value documentation around results that users can inspect,
  while retaining precision, rounding, and bootstrap behavior that affects
  interpretation.
- Preserved necessary methodological terms, including propensity score,
  principal score, always-survivor principal stratum, heterogeneous treatment
  effect, and standardized mean difference.
- Preserved the previously standardized descriptions of `ps_fo`, `prin_fo`,
  `out_fo`, `target_time`, `B`, and `max_attempts`.

## Expanded `cutoff_time` documentation

The `Mapping()` parameter now reads:

> A single finite number giving the last time included in the analysis, in the
> original time scale. The always-survivor principal stratum used in
> treatment-effect analyses is defined by survival through this time.

This wording distinguishes `cutoff_time` from `target_time`: the cutoff defines
the survival-based target population and the end of the analysis window,
whereas `target_time` selects the standardized time points at which
`HTESepT()` estimates effects.

## Function-level revisions

| Documentation topic | User-facing revision |
|---|---|
| `Mapping()` | Explains the mapping as a record of variables and analysis times; clarifies all inputs, the treatment convention, predictor variables, effect modifiers, outcome type, and the role of the cutoff. |
| `DataCheck()` | Describes the checks in terms of data readiness and explains `strict` and each returned status flag in action-oriented language. |
| `DataStandard()` | Describes how IDs, times, and binary values are prepared; explains `drop`, removed observations, stored reports, and precision in user-facing terms. |
| `PSPred()` | States that the result is the estimated probability of treatment `1` for each prediction row. |
| `PrinPred()` | Defines the prediction as survival from baseline through the row's observed time under treatment `a` and explains which observations contribute to successive survival steps. |
| `OutPred()` | States that outcome means are predicted under treatment `a` and survival status `1`, and explains how outcome type selects the regression model. |
| `PSDiag()` | Explains inverse-probability weighting, probability limits, and how changes in absolute SMD reflect covariate balance. |
| `PrinSDiag()` | Explains what the cutoff-time standardized statistics compare and how values near zero should be interpreted. |
| `QR()` | Explains the survival-probability weights, the summaries reported for continuous and binary variables, and the interpretation of `quantile_level`. |
| `ORCI()` | Explains the analyzed treatment group, formula structure, confidence level, reported odds ratios, and the absence of significance-based variable selection. |
| `HTESepT()` | Replaces pseudo-data terminology with a description of combining predictions, fitting separate effect models by time, and using bootstrap resampling. Progress, return values, and numerical safeguards are described in terms of their practical effect. |
| `HTEAllT()` | Parallels `HTESepT()` while clarifying that one trajectory is estimated jointly across all observed times. |
| `SA()` | Explains the added-noise scenarios, the meaning of `ratiovec`, the difference between continuous and binary outcome handling, reproducibility, and the scope of the sensitivity analysis. |
| Package overview | Introduces the target population and workflow directly, clarifies the two HTE functions, and restates assumptions and triple robustness more accessibly. |
| Print, plot, and subset methods | Explains what each method displays or preserves and why edited or subsetted data should be checked again. |

## Files changed

Roxygen sources:

- `R/Mapping.R`
- `R/DataCheck.R`
- `R/DataStandard.R`
- `R/PSPred.R`
- `R/PrinPred.R`
- `R/OutPred.R`
- `R/PSDiag.R`
- `R/PrinSDiag.R`
- `R/QR.R`
- `R/ORCI.R`
- `R/HTESepT.R`
- `R/HTEAllT.R`
- `R/SA.R`
- `R/methods.R`
- `R/PDRobust-package.R`

Generated help files:

- `man/Mapping.Rd`
- `man/DataCheck.Rd`
- `man/DataStandard.Rd`
- `man/PSPred.Rd`
- `man/PrinPred.Rd`
- `man/OutPred.Rd`
- `man/PSDiag.Rd`
- `man/PrinSDiag.Rd`
- `man/QR.Rd`
- `man/ORCI.Rd`
- `man/HTESepT.Rd`
- `man/HTEAllT.Rd`
- `man/SA.Rd`
- `man/pd_methods.Rd`
- `man/PDRobust-package.Rd`

## Validation

- Regenerated all affected `.Rd` files with roxygen2 8.0.0.
- Checked that generated documentation matches the Roxygen source.
- Checked documentation syntax and package examples.
- Compared parsed R expressions before and after the revision to confirm that
  executable code did not change.
- Built the source package with vignettes and ran `R CMD check --as-cran
  --no-manual`: 0 errors, 0 warnings, and 2 notes. The notes were caused by the
  check environment being unable to reach CRAN and external URLs or verify the
  current time; documentation, examples (including `donttest` examples), tests,
  and vignette rebuilding passed.

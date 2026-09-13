# PDRobust audit against The R Journal package-paper guidelines

**Audit date:** 2026-09-13  
**Package version:** 0.3.8  
**Scope:** Current repository source after correcting the `ORCI()` argument and
reviewing the package's print methods. Historical release reports were treated
as records rather than current package behavior.

## Overall judgment

The package itself meets most of the technical package-quality requirements in
The R Journal's current guidance. It has unit tests, complete help topics and
examples for all 13 exported functions, several workflow vignettes, version
control, a bug-reporting route, generated example data with a `data-raw`
script, a standalone website, and a compact set of dependencies that are used
by the source. Its custom result objects now print an introductory sentence and
their main numeric or tabular results, and those containing user-facing plots
also draw the stored plots.

The package is not yet eligible for consideration as an R Journal package paper
because it is not available on CRAN or Bioconductor. A separate R Journal
article and reproducibility bundle are also absent from this repository. These
are release and manuscript tasks rather than defects in the package's
statistical implementation.

## Official criteria used

This audit used the current official [guidelines for papers about R
packages](https://journal.r-project.org/R_package_guidelines.html) and the
official [article submission instructions](https://journal.r-project.org/submissions.html),
both accessed on 2026-09-13. The former distinguishes requirements for
consideration from recommended package practices. The latter adds article
format, reproducibility, accessibility, and submission-bundle requirements.

## Requirements for consideration

| Requirement | Status | Evidence and significance |
|---|---|---|
| Package available on CRAN or Bioconductor | **FAIL** | The network-enabled CRAN incoming check identifies version 0.3.8 as a **new submission**. CRAN or Bioconductor availability is an explicit prerequisite for R Journal consideration. |
| Substantial content of likely interest and use to the R community | **UNCERTAIN** | The package contains a complete longitudinal principal-stratification workflow with validation, prediction, diagnostics, profiling, sensitivity analysis, and HTE estimation. Whether its audience and contribution are sufficiently broad is an editorial judgment that the package source alone cannot establish. |
| Paper demonstrates package use and advantages over competitors or existing approaches | **FAIL** | The repository contains package vignettes and cites the methodological paper, but it does not contain an R Journal article that identifies competing R packages or demonstrates comparative advantages. |
| Unit tests verify functionality | **PASS** | The `testthat` suite covers data contracts, scalability, validation, predictions, diagnostics, bootstrap behavior, return objects, plotting, and error policies. The full suite passed in the fresh source-package check. |
| Every user-facing function is fully documented, with terms, arguments, and examples | **PASS** | All 13 exports have `.Rd` topics containing argument sections and examples. The fresh check passed missing-documentation, code/documentation consistency, usage, contents, examples, and `--run-donttest` checks. |
| Full workflow or example-analysis vignette | **PASS** | Seven source vignettes cover the complete workflow, data preparation, prediction models, diagnostics and sensitivity analysis, treatment-effect estimation, and treatment coding. All vignettes built and rebuilt successfully. |
| Version control | **PASS** | The package is maintained in Git at <https://github.com/whhuan/PD_Robust>. |
| Avoid unnecessary dependencies | **PASS** | The five declared imports are used directly: `ggplot2` for result plots, `quantreg` for weighted quantiles, `rootSolve` for estimating equations, and base-recommended `stats` and `utils` facilities. The dependency checks passed. No dependency was added for the present changes. |

## Recommended package practices

| Practice | Status | Evidence and significance |
|---|---|---|
| Bug-reporting mechanism | **PASS** | `DESCRIPTION` supplies <https://github.com/whhuan/PD_Robust/issues>. |
| User-oriented workflow | **PASS** | The public sequence `Mapping()` -> `DataCheck()` -> `DataStandard()` -> prediction/diagnostic/analysis functions is documented in the README and vignettes. |
| Consistent, meaningful names | **UNCERTAIN** | Names are meaningful and established, but the exported interface mixes acronyms and CamelCase. Renaming it solely for stylistic uniformity would be a breaking change and is not justified for this release. This is a recommended practice, not an eligibility requirement. |
| Consistent code style | **UNCERTAIN** | The source is readable and passed all package checks, but the repository does not declare or enforce a named style guide or formatter. A broad reformat would add review noise and is not needed for CRAN correctness. |
| Object-oriented `print()` and `plot()` methods where appropriate | **PASS** | Nine package-specific print methods cover mapping, validation, diagnostic, odds-ratio, profiling, sensitivity, and both HTE result types. They now introduce the result, print its main table or numeric summary, and draw every stored user-facing plot. Five explicit plot methods return stored diagnostic or HTE plots. `pd_data` and `pd_prediction` deliberately retain normal data-frame and numeric display behavior. |
| Use `message()`/`warning()` rather than `cat()`/`print()` in computational functions | **PASS** | Source searches locate `cat()` and display-oriented `print()` calls only inside the package's print and plot methods. |
| Avoid nonessential file input/output | **PASS** | Exported analysis functions do not read or write user files. |
| Spell logical constants as `TRUE`/`FALSE` | **PASS** | No use of bare `T` or `F` as logical constants was found in package R code. |
| Supply data-generation or cleaning code | **PASS** | `data-raw/generate_example_data.R` documents creation of the bundled data. It remains in version control and is excluded from the CRAN source archive as development material. |
| Standalone documentation website | **PASS** | The pkgdown site is configured at <https://whhuan.github.io/PD_Robust/>. Its URLs passed the network-enabled package check, and the local site was regenerated from the current source. |

## Article and submission-bundle readiness

| Submission item | Status | Evidence and significance |
|---|---|---|
| Article of no more than 20 pages and abstract of no more than 250 words | **FAIL** | No R Journal article source is present. |
| Article prepared with `rjtools`, producing HTML and PDF | **FAIL** | No article directory, `RJournal.tex`, or `RJournal.pdf` is present. The journal currently prefers `rjtools`; the older `rticles` format is deprecated. |
| Fully reproducible article code and data | **FAIL** | Package examples are reproducible, but no article-specific reproduction scripts, data inventory, or evaluated article outputs exist. |
| Accessible figure and table alt text | **UNCERTAIN** | Package vignettes use descriptive figure alternative text in relevant chunks, but an article does not yet exist to audit. |
| Focus on software design, users, performance, limitations, object model, and dependencies | **UNCERTAIN** | The package and existing performance audit provide source material, but only an article draft can demonstrate the required emphasis. |
| Wide-audience introduction and comparison with existing solutions | **FAIL** | No article-level competitor survey or comparison is present. |
| Curated `.bib`, cover letter, `_Rpackages.txt`, and reproducibility files | **FAIL** | These submission artifacts are absent. |
| `rjtools::initial_check_article()` | **FAIL** | The check cannot be run until an article bundle exists and the proposed package is available on CRAN or Bioconductor. |
| Submission size below approximately 10 MB and reproduction below approximately 10 minutes | **UNCERTAIN** | There is no final article bundle to measure. The package's final local vignette rebuild took about 27 seconds, so the package examples themselves are not the current time risk. |
| Article not published or submitted elsewhere | **UNCERTAIN** | This requires an author declaration and cannot be established from the repository. |

## Verification of the reviewed package

A fresh `PDRobust_0.3.8.tar.gz` was built from the current source. A
network-enabled `R CMD check --as-cran` on R 4.4.3 for x86_64 macOS completed
with **0 ERRORs, 0 WARNINGs, and 3 NOTEs**. The notes were:

1. `New submission`, which is expected until the first CRAN release.
2. `unable to verify current time`, which concerns the local checking
   environment rather than package content.
3. HTML math-rendering verification was skipped because the optional `V8`
   package is unavailable in the local check environment.

All package URLs were checked successfully in the network-enabled run. Package
installation, loading, namespace registration, dependencies, R code,
documentation, examples, tests, vignette rebuilds, and PDF and HTML manuals all
passed. Previously completed multi-platform GitHub checks predate the present
interface and print-method edits and are therefore not counted as verification
of this exact working tree.

## Prioritized next work

1. **Release PDRobust on CRAN.** Review the current diff, run the final
   multi-platform checks for this exact commit if desired, and submit version
   0.3.8. R Journal consideration must wait until the package is publicly
   available on CRAN or Bioconductor.
2. **Create the R Journal article with `rjtools`.** Keep the article within 20
   pages and the abstract within 250 words. Generate both HTML and PDF from
   evaluated R code.
3. **Add a defensible comparison with existing R packages and approaches.**
   Identify the closest tools for truncation by death, principal
   stratification, longitudinal HTE analysis, and related diagnostics; explain
   the gap PDRobust fills and demonstrate concrete differences in workflow or
   capability.
4. **Center the article on the software.** Explain the intended users, mapping
   and validation design, S3 result objects, dependency choices, computational
   scaling, supported use cases, and known limitations. Reuse the repository's
   performance audit as evidence rather than repeating the methodological
   paper.
5. **Build one reproducible end-to-end case study.** Include code and
   distributable data, explain treatment coding and the conversion relative to
   Zhang et al. (2026), interpret numeric and graphical outputs, and distinguish
   outcome-noise sensitivity from principal-ignorability sensitivity.
6. **Assemble and check the submission bundle.** Add the curated bibliography,
   cover letter, `_Rpackages.txt`, article source, PDF, reproduction scripts,
   data, and any supplement. Add alternative text, keep the zip near or below
   10 MB, keep reproduction under 10 minutes, and run
   `rjtools::initial_check_article()` after CRAN publication.
7. **Treat naming and formatting changes as optional future work.** Record the
   mixed exported naming convention and absence of an enforced formatter, but
   avoid breaking names or broad formatting changes during this CRAN release.

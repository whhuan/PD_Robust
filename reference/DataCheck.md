# Check whether longitudinal data are ready for analysis

Checks the columns, values, visit structure, and analysis settings
specified by `mapping`. Every observed time from baseline through the
cutoff is treated as an analysis time, and the input data are left
unchanged.

## Usage

``` r
DataCheck(data, mapping, strict = FALSE)
```

## Arguments

- data:

  A long-format data frame.

- mapping:

  A `pd_mapping` object returned by
  [`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md).

- strict:

  If `TRUE`, stop as soon as a problem that prevents analysis is found.
  If `FALSE`, return a report describing all checks that can be
  completed.

## Value

A `pd_data_check` list with the following components:

- valid:

  `TRUE` when no check classified as an error fails. Some warnings about
  encoding or ordering may still prevent analysis.

- ready_for_analysis:

  `TRUE` when the data pass every check required for analysis.

- manual_resolution_required:

  `TRUE` when a failed check requires the user to correct the data
  before standardization.

- can_standardize:

  `TRUE` when no problem requires manual correction. Standardization can
  still fail if rows must be removed but `drop = FALSE`, or if removal
  leaves no observations or only one treatment group.

- checks:

  A data frame with one row per performed check, including the result,
  its importance, details, and a recommended action.

- settings:

  A list containing the validated `mapping`.

- diagnostics:

  Detailed row indices, subject identifiers, and summary tables for the
  performed checks. Missing columns or empty input cause an early return
  with only the checks possible at that stage.

Numeric summaries intended for display are rounded to three decimals;
counts, row indices, identifiers, and logical flags retain their types.

## Examples

``` r
data("BiSample", package = "PDRobust")
map <- Mapping(
  id = "id", time = "time", treatment = "A",
  survival = "S", outcome = "Y",
  baseline_time = 0, cutoff_time = 2,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"), y_type = "B"
)
check <- DataCheck(BiSample, map)
check$ready_for_analysis
#> [1] TRUE
```

# Validate longitudinal principal-stratification data

Uses the column roles, baseline and cutoff endpoints, mapped covariates,
effect modifiers, and outcome type stored in `mapping`. Every actual
observed time within the mapped window belongs to the analysis grid.
Input data are never modified.

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

  Stop when any analysis-blocking check fails.

## Value

A `pd_data_check` object containing `ready_for_analysis`,
`manual_resolution_required`, row-per-check results, settings, and
detailed diagnostics. Calculated display diagnostics are rounded to
three decimals; counts, row indices, identifiers, and logical flags
retain their types.

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

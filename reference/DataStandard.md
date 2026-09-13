# Standardize longitudinal principal-stratification data

Safely converts explicit binary encodings, maps IDs to consecutive
integers, maps the raw analysis time grid to `0, 1, ..., n`, sorts the
panel, and attaches the standardized mapping and audit reports.

## Usage

``` r
DataStandard(data, mapping, drop = FALSE)
```

## Arguments

- data:

  A long-format data frame.

- mapping:

  A `pd_mapping` object returned by
  [`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md).

- drop:

  If `TRUE`, remove unidentifiable rows and entire subjects with
  incomplete baseline-to-cutoff visits or required analysis values.
  Attrition is reported explicitly. If `FALSE`, such problems stop
  standardization.

## Value

A data frame inheriting from `pd_data`, retaining the input column names
and additional unmapped columns. Rows outside the mapped time window are
removed; retained rows are sorted by recoded ID and time. Attributes
are:

- pd_mapping:

  The mapping with standardized baseline and cutoff times; column roles
  retain their input names.

- pd_original_mapping:

  The mapping supplied for the raw data.

- pd_check:

  The final `pd_data_check` report, including attrition. A warning is
  issued if the returned data are not ready for analysis, for example if
  deletion removes one treatment group.

- pd_standardization:

  A list containing `time_map`, `id_map`, `attrition`, and
  `initial_check`. The maps link raw values to their standardized
  values; the initial check describes the input data.

Both the initial and final validation are performed. Audit attributes
describe this standardization call and are not recomputed when the data
are subsequently edited or subsetted; see
[pd_methods](https://whhuan.github.io/PD_Robust/reference/pd_methods.md).
The ID map has one row per retained subject, so audit storage grows with
sample size. Analysis columns and computational mappings retain full
precision; only returned display diagnostics and attrition percentages
are rounded.

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
pd_dat <- DataStandard(BiSample, map)
attr(pd_dat, "pd_mapping")
#> PDRobust data mapping and analysis settings.
#>   ID: id
#>   Time: time
#>   Treatment: A
#>   Survival: S
#>   Outcome: Y
#>   Baseline time: 0
#>   Cutoff time: 2
#>   Mapped covariates: X1, X2, X4
#>   Interest variables: X1, X2
#>   Outcome type: B (binary)
```

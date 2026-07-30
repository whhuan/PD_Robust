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

A `pd_data` frame. Attributes include the standardized mapping, original
mapping, final readiness check, time/ID audit maps, and attrition.

## Examples

``` r
data("BiSample", package = "PDRobust")
map <- Mapping(
  baseline_time = 0, cutoff_time = 2,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"), y_type = "B"
)
pd_dat <- DataStandard(BiSample, map)
attr(pd_dat, "pd_mapping")
#> PDRobust data mapping
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

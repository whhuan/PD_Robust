# Prepare longitudinal data for PDRobust analyses

Converts supported binary values to `0` and `1`, replaces subject IDs
and analysis times with consecutive integers, sorts the data by subject
and time, and stores the information needed by other PDRobust functions.

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

  If `TRUE`, remove rows that cannot be assigned to a subject and remove
  subjects with missing visits or required values between baseline and
  cutoff. The returned report records what was removed. If `FALSE`,
  these problems stop standardization.

## Value

A data frame inheriting from `pd_data`, retaining the input column names
and additional unmapped columns. Rows outside the mapped time window are
removed; retained rows are sorted by recoded ID and time. Attributes
are:

- pd_mapping:

  The mapping updated to use the standardized baseline and cutoff times.
  Column names remain unchanged.

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

The function checks the data both before and after preparation. The
stored reports describe this call and are not recalculated if the
returned data are later edited or subsetted; see
[pd_methods](https://whhuan.github.io/PD_Robust/reference/pd_methods.md).
The ID map has one row per retained subject, so its size increases with
the number of subjects. Analysis values retain full precision; only
summaries shown to users and percentages describing removed data are
rounded.

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

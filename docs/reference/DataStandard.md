<div id="main" class="col-md-9" role="main">

# Standardize longitudinal principal-stratification data

<div class="ref-description section level2">

Safely converts explicit binary encodings, maps IDs to consecutive
integers, maps the raw analysis time grid to `0, 1, ..., n`, sorts the
panel, and attaches the standardized mapping and audit reports.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
DataStandard(data, mapping, drop = FALSE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   data:

    A long-format data frame.

-   mapping:

    A `pd_mapping` object returned by `Mapping()`.

-   drop:

    If `TRUE`, remove unidentifiable rows and entire subjects with
    incomplete baseline-to-cutoff visits or required analysis values.
    Attrition is reported explicitly. If `FALSE`, such problems stop
    standardization.

</div>

<div class="section level2">

## Value

A `pd_data` frame. Attributes include the standardized mapping, original
mapping, final readiness check, time/ID audit maps, and attrition.
Analysis columns and computational mappings retain full precision; only
returned display diagnostics and attrition percentages are rounded.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

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

</div>

</div>

</div>

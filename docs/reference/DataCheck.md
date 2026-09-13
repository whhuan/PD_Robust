<div id="main" class="col-md-9" role="main">

# Validate longitudinal principal-stratification data

<div class="ref-description section level2">

Uses the column roles, baseline and cutoff endpoints, mapped covariates,
effect modifiers, and outcome type stored in `mapping`. Every actual
observed time within the mapped window belongs to the analysis grid.
Input data are never modified.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
DataCheck(data, mapping, strict = FALSE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   data:

    A long-format data frame.

-   mapping:

    A `pd_mapping` object returned by `Mapping()`.

-   strict:

    Stop when any analysis-blocking check fails.

</div>

<div class="section level2">

## Value

A `pd_data_check` object containing `ready_for_analysis`,
`manual_resolution_required`, row-per-check results, settings, and
detailed diagnostics. Calculated display diagnostics are rounded to
three decimals; counts, row indices, identifiers, and logical flags
retain their types.

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
check <- DataCheck(BiSample, map)
check$ready_for_analysis
#> [1] TRUE
```

</div>

</div>

</div>

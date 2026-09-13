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

A `pd_data_check` list with the following components:

-   valid:

    `TRUE` when no check with severity `"error"` fails.
    Analysis-blocking encoding or ordering warnings can still be
    present.

-   ready\_for\_analysis:

    `TRUE` when no analysis-blocking check fails.

-   manual\_resolution\_required:

    `TRUE` when a failed check requires manual correction before
    standardization.

-   can\_standardize:

    The opposite of `manual_resolution_required`. This does not
    guarantee that `DataStandard()` will succeed: deletion may require
    `drop = TRUE`, leave no observations, or remove a treatment group.
    Always inspect the final readiness check.

-   checks:

    A data frame with one row per performed check, including severity,
    blocking and repair flags, details, and recommendations.

-   settings:

    A list containing the validated `mapping`.

-   diagnostics:

    Detailed row indices, subject identifiers, and summary tables for
    the performed checks. Missing columns or empty input cause an early
    return with only the checks possible at that stage.

Calculated display diagnostics are rounded to three decimals; counts,
row indices, identifiers, and logical flags retain their types.

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

<div id="main" class="col-md-9" role="main">

# Define the PDRobust data mapping

<div class="ref-description section level2">

Creates the single source of truth for structural columns, baseline and
cutoff times, prediction-model covariates, effect modifiers, and outcome
type. `target_time` is deliberately not stored in the mapping; it is an
argument of `HTESepT()` only. All ten arguments are required; no
structural role or analysis setting is inferred or defaulted.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
Mapping(
  id,
  time,
  treatment,
  survival,
  outcome,
  baseline_time,
  cutoff_time,
  covariates,
  interest_vars,
  y_type
)
```

</div>

</div>

<div class="section level2">

## Arguments

-   id:

    Character scalar naming the subject ID column.

-   time:

    Character scalar naming the analysis time column.

-   treatment:

    Character scalar naming the treatment column. For causal estimation,
    code the survival-favorable arm as `1` and the other arm as `0`; see
    [PDRobust-package](https://whhuan.github.io/PD_Robust/reference/PDRobust-package.md)
    for the convention and assumptions. Mapping does not infer which arm
    is survival-favorable.

-   survival:

    Character scalar naming the survival/intermediate status column.

-   outcome:

    Character scalar naming the outcome column.

-   baseline\_time:

    One finite numeric baseline time in the raw time scale.

-   cutoff\_time:

    One finite numeric cutoff time in the raw time scale.

-   covariates:

    Character vector naming every non-structural variable used in any
    prediction-model formula.

-   interest\_vars:

    Character vector naming effect modifiers or profiling variables.
    Every entry must also occur in `covariates`.

-   y\_type:

    Outcome type code: `"C"` for continuous or `"B"` for binary.

</div>

<div class="section level2">

## Value

A `pd_mapping` object.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
map <- Mapping(
  id = "id", time = "time", treatment = "A",
  survival = "S", outcome = "Y",
  baseline_time = 3,
  cutoff_time = 9,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"),
  y_type = "C"
)
map
#> PDRobust data mapping and analysis settings.
#>   ID: id
#>   Time: time
#>   Treatment: A
#>   Survival: S
#>   Outcome: Y
#>   Baseline time: 3
#>   Cutoff time: 9
#>   Mapped covariates: X1, X2, X4
#>   Interest variables: X1, X2
#>   Outcome type: C (continuous)
```

</div>

</div>

</div>

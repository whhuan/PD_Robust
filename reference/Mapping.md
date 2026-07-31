# Define the PDRobust data mapping

Creates the single source of truth for structural columns, baseline and
cutoff times, prediction-model covariates, effect modifiers, and outcome
type. `target_time` is deliberately not stored in the mapping; it is an
argument of
[`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md)
only. All ten arguments are required; no structural role or analysis
setting is inferred or defaulted.

## Usage

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

## Arguments

- id:

  Character scalar naming the subject ID column.

- time:

  Character scalar naming the analysis time column.

- treatment:

  Character scalar naming the treatment column.

- survival:

  Character scalar naming the survival/intermediate status column.

- outcome:

  Character scalar naming the outcome column.

- baseline_time:

  One finite numeric baseline time in the raw time scale.

- cutoff_time:

  One finite numeric cutoff time in the raw time scale.

- covariates:

  Character vector naming every non-structural variable used in any
  prediction-model formula.

- interest_vars:

  Character vector naming effect modifiers or profiling variables. Every
  entry must also occur in `covariates`.

- y_type:

  Outcome type code: `"C"` for continuous or `"B"` for binary.

## Value

A `pd_mapping` object.

## Examples

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
#> PDRobust data mapping
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

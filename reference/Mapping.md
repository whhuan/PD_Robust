# Identify variables and analysis times for PDRobust

Records which columns contain the subject ID, time, treatment, survival,
outcome, and covariates, together with the analysis time range and
outcome type. Other package functions use this information to interpret
the data consistently.

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

  A single character string naming the subject ID column.

- time:

  A single character string naming the time column.

- treatment:

  A single character string naming the treatment column. For causal
  estimation, code the survival-favorable arm as `1` and the other arm
  as `0`; see
  [PDRobust-package](https://whhuan.github.io/PD_Robust/reference/PDRobust-package.md)
  for the convention and assumptions. The function does not determine
  which arm is survival-favorable.

- survival:

  A single character string naming the column that records survival or
  another intermediate status.

- outcome:

  A single character string naming the outcome column.

- baseline_time:

  A single finite number giving the baseline time in the original time
  scale.

- cutoff_time:

  The time point, on the original time scale, at which the
  always-survivor principal stratum is defined.

- covariates:

  A character vector naming all variables used as predictors in the
  propensity score, principal score, or outcome mean models.

- interest_vars:

  A character vector specifying the names of variables used to evaluate
  heterogeneous treatment effects. Each variable must also be included
  in `covariates`.

- y_type:

  The outcome type: `"C"` for continuous or `"B"` for binary.

## Value

A `pd_mapping` object that can be supplied to
[`DataCheck()`](https://whhuan.github.io/PD_Robust/reference/DataCheck.md)
and
[`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md).

## Details

All ten arguments are required. `target_time` is specified separately
when calling
[`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md)
because it selects time points for that analysis rather than describing
the data.

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

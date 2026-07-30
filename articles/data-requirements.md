# Data requirements and standardization

## Mapping raw columns and times

[`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md)
is the sole source of truth for structural columns, the raw analysis
window, nuisance-model covariates, effect modifiers, and outcome type.

``` r

map <- Mapping(
  id = "id",
  time = "time",
  treatment = "A",
  survival = "S",
  outcome = "Y",
  baseline_time = 3,
  cutoff_time = 9,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"),
  y_type = "C"
)

class(map)
map
```

Every `interest_vars` entry must also occur in `covariates`.
`target_time` is not stored in the mapping; it is supplied only to
[`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md).

## Validation without modification

``` r

data("ImperfectConSample", package = "PDRobust")
raw <- ImperfectConSample
check <- DataCheck(raw, map, strict = FALSE)

check$ready_for_analysis
check$manual_resolution_required
check$checks
check$diagnostics
print(check)
```

[`DataCheck()`](https://whhuan.github.io/PD_Robust/reference/DataCheck.md)
assesses required columns, type conversion, binary encodings, missing
IDs or times, duplicated subject-time records, treatment consistency,
visit completeness, monotone survival, structural outcome missingness
after death, missing outcomes among survivors, covariate completeness,
and outcome type. Each check includes an action-oriented message. With
`strict = TRUE`, an analysis-blocking failure raises an error after the
report is constructed.

## Standardization and audit attributes

``` r

pd_data <- DataStandard(raw, map, drop = TRUE)

class(pd_data)
attr(pd_data, "pd_mapping")
attr(pd_data, "pd_original_mapping")
standardization <- attr(pd_data, "pd_standardization")
standardization$time_map
standardization$id_map
standardization$attrition
attr(pd_data, "pd_check")
```

[`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md)
retains every actual observed time inside the mapped window and maps the
ordered time grid to `0, 1, ..., n`. IDs are mapped to consecutive
integers, explicit binary encodings are converted safely, the panel is
sorted, and audit information is attached. Subjects must have one usable
record at each retained time. With `drop = TRUE`, deletions are
subject-level and recorded in the attrition report; structural outcome
missingness after death is not treated as ordinary survivor outcome
missingness.

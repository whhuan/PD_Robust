# Data requirements and standardization

## Introduction

The complete workflow is illustrated as follows. This article focuses on
the package’s data requirements and the first three steps, including
functions
[`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md),
[`DataCheck()`](https://whhuan.github.io/PD_Robust/reference/DataCheck.md)
and
[`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md).

``` text
data -> Mapping() -> DataCheck() -> DataStandard()
     -> prediction / diagnostic / analysis functions
```

``` r

library(PDRobust)
data("ImperfectConSample", package = "PDRobust")
data("BiSample", package = "PDRobust")
```

Two built-in datasets are included with the package. The datasets are
stored in the `data/` directory, and the scripts used to generate them
are provided in the `data-raw/` directory.

The first dataset, `BiSample`, is a standardized longitudinal dataset
that satisfies the package’s data requirements. It contains no
nonstructural missing values; outcome values are missing only when they
are structurally unobservable due to any kind of truncation. Each row
represents one subject at a specific time. The dataset includes a
subject identifier (`id`), assessment time (`time`), survival status
(`S`), treatment assignment (`A`), a binary outcome (`Y`), and six
subject-level covariates (`X1`–`X6`) .

``` r

data("BiSample", package = "PDRobust")
head(BiSample)
#>   id time    Pi S1 S0 S A Y1 Y0 Y    X1     X2     X3 X4 X5 X6
#> 1  1    0 0.987  1  1 1 1  0  1 0 1.479 -0.168  0.873  0  1  1
#> 2  1    1 0.987  1  1 1 1  0  0 0 1.479 -0.168  0.873  0  1  1
#> 3  1    2 0.987  1  1 1 1  0  0 0 1.479 -0.168  0.873  0  1  1
#> 4  2    0 0.777  1  1 1 1  0  0 0 0.267  0.350 -1.438  1  1  1
#> 5  2    1 0.777  1  1 1 1  0  0 0 0.267  0.350 -1.438  1  1  1
#> 6  2    2 0.777  1  1 1 1  0  0 0 0.267  0.350 -1.438  1  1  1
```

The second one, `ImperfectConSample`, is designed to resemble
longitudinal data collected in a clinical trial with repeated follow-up
assessments. Each row represents a patient observation at a scheduled
visit. The dataset includes a patient identifier (`patient_id`), visit
time (`visit_month`), survival status (`alive_status`), treatment
assignment (`treatment`), a continuous clinical outcome, and six
patient-level covariates (`X1`–`X6`) representing relevant clinical and
demographic information.

``` r

head(ImperfectConSample)
#>   patient_id visit_month alive_status treatment clinical_outcome     X1     X2
#> 1    PT-0171           0            1         1            4.598  1.452 -2.075
#> 2    PT-0100           6            0         1               NA  1.473 -0.758
#> 3    PT-0056           0            1         0            8.806 -2.722 -0.735
#> 4    PT-0034           6            1         0           13.851 -1.471  0.278
#> 5    PT-0164          12            1         1            9.643 -1.272 -1.881
#> 6    PT-0058           0            1         1           10.341 -0.534 -0.842
#>       X3 X4 X5 X6
#> 1 -0.147  0  1  1
#> 2  0.608  0  1  1
#> 3  0.424  1  1  0
#> 4 -0.158  0  0  0
#> 5 -3.333  0  1  0
#> 6 -0.092  0  1  0
```

## Mapping raw columns and times

[`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md)
is the sole source of truth for structural columns, analysis window on
the raw time scale, the covariated used to estimate treatment effects,
the variables of interest, and the outcome type.

The arguments `id, time, treatment, survival, outcome` specify the
corresponding column names in the input dataset.

The arguments `baseline_time` and `cutoff_time` define the beginning and
end of the analysis window, respectively. Each must be specified as a
single finite numeric value on the raw time scale, with
`baseline_time < cutoff_time`. For example, when obeserved time points
are (0, 1, …, 4) may be set to 0 or 1, whereas the cutoff may be set to
a later time point, such as 4. For `ImperfectConSample`, the observed
time points are (0, 6, 12); therefore, the analysis window is defined
using `baseline_time = 0, cutoff_time = 12`.

The argument `covariates` and `interest_vars` are character vectors
containing the column names of the relevant covariates. Variables
specified in `interest_vars` must also be included in `covariates` . The
argument `y_type` specifies the outcome type. It should be set to `B`
for a binary outcome and `C` for a continuous outcome.

``` r

map <- Mapping(
  id = "patient_id",
  time = "visit_month",
  treatment = "treatment",
  survival = "alive_status",
  outcome = "clinical_outcome",
  baseline_time = 0,
  cutoff_time = 12,
  covariates = paste0("X", 1:6),
  interest_vars = c("X1", "X2"),
  y_type = "C" # "B"
)
```

Users can inspect the mapping details using
[`print()`](https://rdrr.io/r/base/print.html) and the
[`attributes()`](https://rdrr.io/r/base/attributes.html) function may
additionally be used to inspect object-level metadata, such as its
class.

``` r

print(map)
#> PDRobust data mapping
#>   ID: patient_id
#>   Time: visit_month
#>   Treatment: treatment
#>   Survival: alive_status
#>   Outcome: clinical_outcome
#>   Baseline time: 0
#>   Cutoff time: 12
#>   Mapped covariates: X1, X2, X3, X4, X5, X6
#>   Interest variables: X1, X2
#>   Outcome type: C (continuous)
```

``` r

attributes(map)
#> $names
#>  [1] "id_col"        "time_col"      "A_col"         "S_col"        
#>  [5] "Y_col"         "baseline_time" "cutoff_time"   "covariates"   
#>  [9] "interest_vars" "y_type"       
#> 
#> $class
#> [1] "pd_mapping"
```

## Validation without modification

[`DataCheck()`](https://whhuan.github.io/PD_Robust/reference/DataCheck.md)
evaluates whether the input dataset satisfies the structural and
analytical requirements of `PDRobust` without altering the data. It
identifies potential issues, reports their severity and recommended
handling, and determines whether the dataset is ready for analysis, can
be standardized using
[`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md),
or requires manual resolution.

Each check includes an action-oriented message. When `strict = FALSE`,
which is the default,
[`DataCheck()`](https://whhuan.github.io/PD_Robust/reference/DataCheck.md)
returns the complete validation report without modifying the input
dataset. When `strict = TRUE`, the function raises an error if one or
more failed checks are marked as analysis-blocking.

``` r

check <- DataCheck(ImperfectConSample, map, strict = FALSE)

attributes(check)
#> $names
#> [1] "valid"                      "ready_for_analysis"        
#> [3] "manual_resolution_required" "can_standardize"           
#> [5] "checks"                     "settings"                  
#> [7] "diagnostics"               
#> 
#> $class
#> [1] "pd_data_check"
```

[`DataCheck()`](https://whhuan.github.io/PD_Robust/reference/DataCheck.md)
returns an object of class `pd_data_check`. The object contains the
following components:

| Component | Description |
|----|----|
| `valid` | Indicates whether all checks with severity `"error"` have passed. Informational messages and warnings do not by themselves make the report invalid. |
| `ready_for_analysis` | Indicates whether the dataset can be used directly in downstream prediction, diagnostic, and analysis functions. It is `TRUE` only when no failed check is marked as analysis-blocking. |
| `manual_resolution_required` | Indicates whether at least one failed check requires manual review or correction. Such issues are not automatically resolved by [`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md). |
| `can_standardize` | Indicates whether [`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md) may proceed. It is `TRUE` when no failed check requires manual resolution, although subject-level deletion may still require `drop = TRUE`. |
| `checks` | A data frame containing the itemized validation results, including the status, severity, diagnostic summary, analysis implications, and recommended handling for each check. |
| `settings` | Records the settings used during validation. The current implementation stores the validated mapping object in `settings$mapping`. |
| `diagnostics` | Contains detailed supporting information, such as affected row numbers, subject identifiers, missingness summaries, treatment-group counts, and problematic covariates. |

``` r

check$valid
#> [1] FALSE
check$ready_for_analysis
#> [1] FALSE
check$manual_resolution_required
#> [1] FALSE
check$can_standardize
#> [1] TRUE
```

The following are diagnostics of check for `ImperfectConSample`. Users
can find detailed descriptions of all validation items in the article
*Details-for-DataCheck*.

``` r

head(check$diagnostics)
#> $missing_id_time_rows
#> [1] 208
#> 
#> $duplicate_rows
#> integer(0)
#> 
#> $duplicate_subjects
#> character(0)
#> 
#> $missing_by_time
#>   time missing_subjects
#> 1    0                0
#> 2    6                1
#> 3   12                1
#> 
#> $incomplete_subjects
#> [1] "PT-0001" "PT-0004"
#> 
#> $treatment_invalid_rows
#> integer(0)
```

## Standardization and audit attributes

[`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md)
returns a prepared data for later analysis.

``` r

pd_data <- DataStandard(ImperfectConSample, map, drop = TRUE)
class(pd_data)
#> [1] "pd_data"    "data.frame"
head(pd_data)
#>   patient_id visit_month alive_status treatment clinical_outcome     X1     X2
#> 1          1           0            1         1           10.803  0.168  0.421
#> 2          1           1            1         1           12.006  0.168  0.421
#> 3          1           2            1         1            7.833  0.168  0.421
#> 4          2           0            1         0            4.101 -2.400 -0.324
#> 5          2           1            1         0            5.508 -2.400 -0.324
#> 6          2           2            0         0               NA -2.400 -0.324
#>       X3 X4 X5 X6
#> 1 -0.557  1  1  1
#> 2 -0.557  1  1  1
#> 3 -0.557  1  1  1
#> 4 -0.391  0  0  0
#> 5 -0.391  0  0  0
#> 6 -0.391  0  0  0
```

The returned `pd_data` object contains several attributes that document
its structure and the transformations applied during standardization.
These attributes can be inspected using:

``` r

names(attributes(pd_data))
#> [1] "names"               "row.names"           "class"              
#> [4] "pd_mapping"          "pd_original_mapping" "pd_check"           
#> [7] "pd_standardization"
```

| Attribute | Description |
|:---|:---|
| `names` | Stores the column names of the standardized data frame. |
| `row.names` | Stores the row identifiers used by the data frame. |
| `class` | Identifies the object classes, including its data-frame and package-specific classes. |
| `pd_mapping` | Stores the standardized mapping used by subsequent package functions, including the standardized column roles and analysis-time specification. |
| `pd_original_mapping` | Preserves the original user-supplied mapping on the raw data scale before identifiers, time points, and encodings were standardized. |
| `pd_check` | Stores the validation results associated with the standardized dataset, including readiness indicators, detected issues, and recommended handling. |
| `pd_standardization` | Records the principal transformations performed during standardization, such as identifier and time mappings, encoding conversions, retained observations, and subject-level exclusions. |

For exmaple, the original and standardized mappings can be compared
using:

``` r

attr(pd_data, "pd_original_mapping")
#> PDRobust data mapping
#>   ID: patient_id
#>   Time: visit_month
#>   Treatment: treatment
#>   Survival: alive_status
#>   Outcome: clinical_outcome
#>   Baseline time: 0
#>   Cutoff time: 12
#>   Mapped covariates: X1, X2, X3, X4, X5, X6
#>   Interest variables: X1, X2
#>   Outcome type: C (continuous)
attr(pd_data, "pd_mapping")
#> PDRobust data mapping
#>   ID: patient_id
#>   Time: visit_month
#>   Treatment: treatment
#>   Survival: alive_status
#>   Outcome: clinical_outcome
#>   Baseline time: 0
#>   Cutoff time: 2
#>   Mapped covariates: X1, X2, X3, X4, X5, X6
#>   Interest variables: X1, X2
#>   Outcome type: C (continuous)
```

Among these attributes, `pd_standardization` is particularly important
because it provides the primary audit trail for the changes made to the
input dataset. It can be inspected directly using:

``` r

standardization <- attr(pd_data, "pd_standardization")
names(standardization)
#> [1] "time_map"      "id_map"        "attrition"     "initial_check"
```

It retains all observed assessment times within the analysis window
defined by the mapping object and transforms the ordered time grid to
consecutive integers (0, 1, …, n).

``` r

standardization$time_map
#>   raw_time standardized_time
#> 1        0                 0
#> 2        6                 1
#> 3       12                 2
```

Subject identifiers are similarly mapped to consecutive integers,
explicitly recognized binary encodings are converted safely, and the
resulting longitudinal dataset is sorted by subject and standardized
analysis time.

For a subject to be retained, the dataset must contain one usable record
at each retained assessment time.

``` r

head(standardization$id_map)
#>    raw_id standardized_id
#> 1 PT-0005               1
#> 2 PT-0006               2
#> 3 PT-0007               3
#> 4 PT-0008               4
#> 5 PT-0009               5
#> 6 PT-0010               6
```

When `drop = TRUE`, exclusions are applied at the subject level rather
than to individual records, and all exclusions are documented in the
attrition report. Outcome values that are structurally unobservable
after death or other trunction are distinguished from ordinary missing
outcomes among surviving subjects and are therefore handled separately
during validation and standardization.

``` r

standardization$attrition
#> $original_rows
#> [1] 599
#> 
#> $rows_outside_analysis_window
#> [1] 0
#> 
#> $unidentified_rows_removed
#> [1] 1
#> 
#> $original_subjects
#> [1] 200
#> 
#> $removed_subjects
#> [1] "PT-0001" "PT-0004" "PT-0002" "PT-0003"
#> 
#> $removed_subjects_by_reason
#>   subject                          reason
#> 1 PT-0001          missing_analysis_visit
#> 2 PT-0004          missing_analysis_visit
#> 3 PT-0002 missing_required_analysis_value
#> 4 PT-0003 missing_required_analysis_value
#> 
#> $retained_subjects
#> [1] 196
#> 
#> $retained_percent
#> [1] 98
```

Finally, even when a built-in dataset such as `BiSample`, or a
user-supplied dataset, already satisfies all `PDRobust` data
requirements, it should still be processed through the package’s
data-preparation workflow before analysis. This ensures that the dataset
is formally validated, standardized, and supplied with the mapping and
audit attributes required by downstream functions.

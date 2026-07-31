# Imperfect Continuous Longitudinal Example Data

A deliberately imperfect continuous-outcome longitudinal data set
derived from the analysis-ready `ConSample` data. The data mimic common
issues encountered in raw clinical data exports while remaining
recoverable using
[`DataCheck`](https://whhuan.github.io/PD_Robust/reference/DataCheck.md)
and
[`DataStandard`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md)
with `drop = TRUE`.

## Usage

``` r
ImperfectConSample
```

## Format

A data frame in long format with one row per subject and visit,
containing the following variables:

- `patient_id`:

  Noncanonical character subject identifier.

- `visit_month`:

  Character-encoded visit time in months.

- `treatment`:

  Character-encoded binary treatment assignment.

- `alive_status`:

  Character-encoded binary survival or intermediate status.

- X1, X2, X3:

  Continuous baseline covariates.

- X4, X5, X6:

  Binary baseline covariates.

- `clinical_outcome`:

  Continuous longitudinal clinical outcome.

## Source

Simulated for package examples.

## Details

The data include nonstandard subject identifiers, character-encoded
visit times and binary variables, unsorted records, an incomplete
longitudinal record, missing required covariate values, a missing
outcome among survivors, and a record with a missing subject identifier.
Structural outcome missingness for records with `alive_status = 0` is
retained.

## Examples

``` r
data("ImperfectConSample", package = "PDRobust")
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

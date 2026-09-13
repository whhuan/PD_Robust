# Binary longitudinal example data

Simulated long-format data for illustrating binary-outcome analyses.
Potential survival is generated with \\S^1 \ge S^0\\, matching the
package's treatment-1 survival-favorable convention. The simulation
variables are not observed counterfactual information available in a
real study.

## Usage

``` r
BiSample
```

## Format

A simulated long-format data frame with 1,200 rows (400 subjects at
three visits) and 16 variables:

- id:

  Subject identifier.

- time:

  Analysis time.

- Pi:

  Simulated probability of treatment 1 conditional on baseline
  covariates, stored to three decimal places.

- S1, S0:

  Simulated potential survival indicators under treatment 1 and 0,
  respectively; 1 denotes alive and 0 denotes dead.

- Y1, Y0:

  Simulated binary potential outcomes under treatment 1 and 0,
  respectively. These simulation variables are retained for
  illustration; package analyses use the observed outcome `Y`.

- X1, X2, X3:

  Continuous baseline covariates.

- X4, X5, X6:

  Binary baseline covariates.

- A:

  Binary treatment indicator.

- S:

  Binary survival or intermediate-status indicator.

- Y:

  Binary outcome, structurally missing after death.

## Source

Simulated for package examples.

## Examples

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

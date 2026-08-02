# Binary longitudinal example data

Simulated long-format data for illustrating binary-outcome analyses.

## Usage

``` r
BiSample
```

## Format

A simulated long-format data frame and the following variables:

- id:

  Subject identifier.

- time:

  Analysis time.

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
#>   id time S A Y    X1     X2     X3 X4 X5 X6
#> 1  1    0 1 1 0 1.479 -0.168  0.873  0  1  1
#> 2  1    1 1 1 0 1.479 -0.168  0.873  0  1  1
#> 3  1    2 1 1 0 1.479 -0.168  0.873  0  1  1
#> 4  2    0 1 1 0 0.267  0.350 -1.438  1  1  1
#> 5  2    1 1 1 0 0.267  0.350 -1.438  1  1  1
#> 6  2    2 1 1 0 0.267  0.350 -1.438  1  1  1
```

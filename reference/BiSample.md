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
#>   id time S A Y     X1     X2     X3 X4 X5 X6
#> 1  1    0 1 1 0  1.521  0.261  0.649  0  1  0
#> 2  1    1 1 1 0  1.521  0.261  0.649  0  1  0
#> 3  1    2 1 1 1  1.521  0.261  0.649  0  1  0
#> 4  2    0 1 1 0 -0.395 -1.241 -0.115  0  1  0
#> 5  2    1 1 1 0 -0.395 -1.241 -0.115  0  1  0
#> 6  2    2 1 1 0 -0.395 -1.241 -0.115  0  1  0
```

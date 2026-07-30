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

- Pi:

  Simulated treatment probability.

- S1, S0:

  Potential survival indicators under treatment and control.

- logit_Y1, logit_Y0:

  Potential-outcome linear predictors.

- prob_Y1, prob_Y0:

  Potential binary-outcome probabilities.

- Y1, Y0:

  Potential binary outcomes.

- S:

  Binary survival or intermediate-status indicator.

- Y:

  Binary outcome, structurally missing after death.

- U11:

  Always-survivor indicator `S1 * S0`.

- S1minusS0:

  Difference between potential survival indicators.

## Source

Simulated for package examples.

## Examples

``` r
data("BiSample", package = "PDRobust")
head(BiSample)
#>   id time         X1        X2         X3 X4 X5 X6 A        Pi S1 S0   logit_Y1
#> 1  1    0  1.5205087  0.261479  0.6488924  0  1  0 1 0.9808431  1  1 -1.0310761
#> 2  1    1  1.5205087  0.261479  0.6488924  0  1  0 1 0.9808431  1  1 -0.9310761
#> 3  1    2  1.5205087  0.261479  0.6488924  0  1  0 1 0.9808431  1  1 -0.8310761
#> 4  2    0 -0.3950091 -1.241097 -0.1151943  0  1  0 1 0.9272987  1  1 -2.2067618
#> 5  2    1 -0.3950091 -1.241097 -0.1151943  0  1  0 1 0.9272987  1  0 -2.1067618
#> 6  2    2 -0.3950091 -1.241097 -0.1151943  0  1  0 1 0.9272987  1  0 -2.0067618
#>     logit_Y0    prob_Y1    prob_Y0 Y1 Y0 S Y U11 S1minusS0
#> 1 -1.3741641 0.26287554 0.20194791  0  0 1 0   1         0
#> 2 -1.1741641 0.28270646 0.23610313  1  1 1 1   1         0
#> 3 -0.9741641 0.30341759 0.27405129  1  0 1 1   1         0
#> 4 -2.1316317 0.09914492 0.10606018  0  0 1 0   1         0
#> 5 -3.9316317 0.10844135 0.01923443  0  0 1 0   0         1
#> 6 -3.7316317 0.11849480 0.02339336  0  0 1 0   0         1
```

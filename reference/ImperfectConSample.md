# Imperfect continuous longitudinal example data

A deliberately imperfect long-format continuous-outcome data frame for
demonstrating validation and standardization diagnostics.

## Usage

``` r
ImperfectConSample
```

## Format

A deliberately imperfect long-format data frame with the following
variables:

- id:

  Noncanonical subject identifier.

- time:

  Raw analysis time encoded as character values 3, 6, and 9.

- X1, X2, X3:

  Continuous baseline covariates.

- X4, X5, X6:

  Binary baseline covariates.

- A:

  Binary treatment indicator encoded as character values.

- Pi:

  Simulated treatment probability.

- S1, S0:

  Potential survival indicators under treatment and control.

- EY1, EY0:

  Potential continuous-outcome conditional means.

- Y1, Y0:

  Potential continuous outcomes.

- S:

  Observed survival encoded as character values.

- Y:

  Observed continuous outcome, structurally missing after death.

- U11:

  Always-survivor indicator `S1 * S0`.

- S1minusS0:

  Difference between potential survival indicators.

## Source

Simulated for package validation examples.

## Examples

``` r
data("ImperfectConSample", package = "PDRobust")
head(ImperfectConSample)
#>            id time         X1         X2          X3 X4 X5 X6 A        Pi S1 S0
#> 1 subject_171    3  0.4540710 -0.2774896  0.90993870  1  0  0 1 0.9050652  1  1
#> 2 subject_100    6  0.5014679  1.0781961 -0.53812324  1  1  1 1 0.8431193  1  1
#> 3  subject_56    3 -0.5335556 -0.8424094 -0.09196662  0  1  0 1 0.9080071  1  1
#> 4  subject_34    6 -1.5447306  0.8740341 -0.03008102  1  1  0 1 0.5820402  1  1
#> 5 subject_164    9  0.4218631  1.1140381 -0.08442510  1  0  0 1 0.7457024  1  1
#> 6  subject_58    3 -0.7972671  1.3252652 -0.16084732  0  1  0 1 0.7631763  1  1
#>        EY1      EY0       Y1        Y0 S        Y U11 S1minusS0
#> 1 8.325225 7.616573 6.643768 12.670851 1 6.643768   1         0
#> 2 8.981719 8.177565 7.286246 10.532920 1 7.286246   1         0
#> 3 7.302970 6.849763 6.419524  3.252127 1 6.419524   1         0
#> 4 8.586194 8.056272 3.165516  9.772680 1 3.165516   1         0
#> 5 9.072174 8.527027 7.837081  6.435173 1 7.837081   1         0
#> 6 8.090926 7.454211 1.584122  6.137023 1 1.584122   1         0
```

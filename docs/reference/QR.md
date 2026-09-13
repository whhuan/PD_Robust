<div id="main" class="col-md-9" role="main">

# Summarize cutoff covariates in the always-survivor principal stratum

<div class="ref-description section level2">

Restores the original weighted intercept-only quantile-regression
algorithm. For each nonbinary mapped interest variable,
`quantreg::rq(variable ~ 1, weights = K_p0, tau = quantile_level)` is
fitted on cutoff rows.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
QR(data, prin_fo, quantile_level = 0.5)
```

</div>

</div>

<div class="section level2">

## Arguments

-   data:

    A standardized `pd_data` object.

-   prin\_fo:

    Principal-score formula.

-   quantile\_level:

    Quantile probabilities strictly between zero and one.

</div>

<div class="section level2">

## Value

A `QR` object containing three-decimal weighted means and quantiles;
principal-score weights retain full precision.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
# \donttest{
data("BiSample", package = "PDRobust")
map <- Mapping(
  id = "id", time = "time", treatment = "A",
  survival = "S", outcome = "Y",
  baseline_time = 0, cutoff_time = 2,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"), y_type = "B"
)
pd_dat <- DataStandard(BiSample, map)
result <- QR(
  pd_dat,
  S ~ X1 + X2 + X4 + A + time,
  quantile_level = c(0.25, 0.5, 0.75)
)
result$mean
#>    X1    X2 
#> 0.140 0.141 
# }
```

</div>

</div>

</div>

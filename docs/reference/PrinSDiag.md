<div id="main" class="col-md-9" role="main">

# Diagnose principal-score balance

<div class="ref-description section level2">

Refits the propensity and principal-score models internally. Estimated
propensity scores are always clipped to `[0.01, 0.99]`. Cumulative
principal scores use all actual observed times from baseline through
cutoff, and the diagnostic equation is evaluated at cutoff using the
original algorithm.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
PrinSDiag(data, ps_fo, prin_fo)
```

</div>

</div>

<div class="section level2">

## Arguments

-   data:

    A standardized `pd_data` object.

-   ps\_fo:

    Propensity-score formula; its numeric covariates are diagnosed.

-   prin\_fo:

    Principal-score formula.

</div>

<div class="section level2">

## Value

A `PrinSDiag` object containing three-decimal standardized statistics
and plots; nuisance probabilities retain full precision.

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
result <- PrinSDiag(
  pd_dat,
  A ~ X1 + X2 + X4,
  S ~ X1 + X2 + X4 + A + time
)
result$statistics
#>     X1     X2     X4 
#> -1.182  0.869 -0.074 
# }
```

</div>

</div>

</div>

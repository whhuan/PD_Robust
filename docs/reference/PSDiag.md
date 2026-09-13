<div id="main" class="col-md-9" role="main">

# Diagnose propensity-score covariate balance

<div class="ref-description section level2">

Fits the propensity-score model internally on baseline observations,
clips every estimated propensity score to `[0.01, 0.99]`, creates
ordinary inverse-probability-of-treatment weights, and evaluates balance
using the original pooled and weighted-ESS SMD denominators.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
PSDiag(data, ps_fo)
```

</div>

</div>

<div class="section level2">

## Arguments

-   data:

    A standardized `pd_data` object.

-   ps\_fo:

    Propensity-score formula.

</div>

<div class="section level2">

## Value

A `PSDiag` object containing three-decimal SMD summaries and a plot;
propensity scores and weights retain full precision.

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
result <- PSDiag(pd_dat, A ~ X1 + X2 + X4)
result$smd_after
#>     X1     X2     X4 
#> -0.197 -0.047  0.056 
# }
```

</div>

</div>

</div>

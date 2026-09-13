<div id="main" class="col-md-9" role="main">

# Estimate treatment-group-specific survival odds ratios at cutoff

<div class="ref-description section level2">

Fits the supplied logistic model among subjects in the selected
treatment group at the mapped cutoff time.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
ORCI(data, fomula, a, conf_level = 0.95)
```

</div>

</div>

<div class="section level2">

## Arguments

-   data:

    A standardized `pd_data` object.

-   fomula:

    Logistic-regression formula with the mapped survival column as its
    response. The spelling `fomula` is retained for compatibility; use
    this spelling when supplying the argument by name.

-   a:

    Required cutoff treatment group, exactly `0` or `1`.

-   conf\_level:

    Confidence level.

</div>

<div class="section level2">

## Value

An `odds_ratios` object containing three-decimal odds-ratio summaries, a
full-precision fitted model, model diagnostics, and a plot.

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
result <- ORCI(
  pd_dat, S ~ X1 + X2 + X4, a = 0
)
result$forestplotdat
#>    covname estcoef lowerbd upperbd
#> X1      X1   1.788   1.021   3.132
#> X2      X2   1.834   0.988   3.404
#> X4      X4   2.028   0.677   6.076
# }
```

</div>

</div>

</div>

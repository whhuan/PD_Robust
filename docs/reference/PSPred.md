<div id="main" class="col-md-9" role="main">

# Estimate propensity scores

<div class="ref-description section level2">

Fits a logistic propensity-score model on baseline observations from
`fit_dat` and predicts on every row of `pred_dat`. The model is refitted
on every call; no fitted object or cache is retained.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
PSPred(ps_fo, fit_dat, pred_dat, mapping, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   ps\_fo:

    Propensity-score formula.

-   fit\_dat:

    Data used to fit the model.

-   pred\_dat:

    Data on which to predict.

-   mapping:

    A `pd_mapping` object. It supplies column names and `baseline_time`
    only; it never supplies data.

-   ...:

    Additional arguments passed to `stats::glm()`.

</div>

<div class="section level2">

## Value

A numeric vector of class `pd_prediction` with length `nrow(pred_dat)`,
rounded to three decimal places after prediction.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data("BiSample", package = "PDRobust")
map <- Mapping(
  id = "id", time = "time", treatment = "A",
  survival = "S", outcome = "Y",
  baseline_time = 0, cutoff_time = 2,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"), y_type = "B"
)
pd_dat <- DataStandard(BiSample, map)
ps <- PSPred(A ~ X1 + X2 + X4, pd_dat, pd_dat, map)
head(ps)
#> [1] 0.942 0.942 0.942 0.863 0.863 0.863
```

</div>

</div>

</div>

<div id="main" class="col-md-9" role="main">

# Diagnostics, profiling, and sensitivity analysis

<div id="cb1" class="sourceCode">

``` r
library(PDRobust)
data("BiSample", package = "PDRobust")
raw <- BiSample
map <- Mapping(
  id = "id",
  time = "time",
  treatment = "A",
  survival = "S",
  outcome = "Y",
  baseline_time = 0,
  cutoff_time = 2,
  covariates = c("X1", "X2", "X4"),
  interest_vars = c("X1", "X2"),
  y_type = "B"
)
pd_data <- DataStandard(raw, map)
ps_fo <- A ~ X1 + X2 + X4
prin_fo <- S ~ X1 + X2 + X4 + A + time
```

</div>

<div class="section level2">

## Propensity-score balance

<div id="cb2" class="sourceCode">

``` r
ps_diag <- PSDiag(pd_data, ps_fo)
ps_diag$smd_before
ps_diag$smd_after
ps_diag$weights
ps_diag$propensity
plot(ps_diag)
```

</div>

`PSDiag()` uses the original pooled denominator before weighting and the
weighted effective-sample-size denominator after ordinary IPTW. Before
weights are calculated, it always executes
`pi <- pmin(pmax(pi, 0.01), 0.99)`.

</div>

<div class="section level2">

## Principal-score balance

<div id="cb3" class="sourceCode">

``` r
prin_diag <- PrinSDiag(pd_data, ps_fo, prin_fo)
prin_diag$statistics
prin_diag$p0
prin_diag$p1
prin_diag$propensity
plot(prin_diag)
```

</div>

`PrinSDiag()` evaluates the original diagnostic equation at cutoff using
cumulative principal probabilities. Its propensity scores are also
always clipped to `[0.01, 0.99]` before the diagnostic denominators are
formed.

</div>

<div class="section level2">

## Principal-stratum summaries

<div id="cb4" class="sourceCode">

``` r
profile <- QR(
  pd_data,
  prin_fo,
  quantile_level = c(0.25, 0.5, 0.75)
)
profile$mean
profile$quantile
profile$weights
```

</div>

`QR()` uses cutoff principal-score weights. Means are weighted directly,
and quantiles are estimated using weighted intercept-only
`quantreg::rq()` models.

</div>

<div class="section level2">

## Treatment-group odds ratios

<div id="cb5" class="sourceCode">

``` r
or0 <- ORCI(pd_data, S ~ X1 + X2 + X4, a = 0)
or1 <- ORCI(pd_data, S ~ X1 + X2 + X4, a = 1)
or0$forestplotdat
or0$model
plot(or0)
```

</div>

`ORCI()` fits a cutoff logistic model within the selected treatment
group and returns exponentiated coefficients, confidence intervals, the
fitted model, analysis data, settings, and a forest plot.

</div>

<div class="section level2">

## Binary- and continuous-outcome sensitivity analysis

<div id="cb6" class="sourceCode">

``` r
set.seed(20260728)
binary_sa <- SA(
  pd_data,
  A ~ X1 + X2 + X4,
  S ~ X1 + X2 + X4 + A + time,
  Y ~ X1 + X2 + A,
  ratiovec = c(0, 0.02)
)

data("ImperfectConSample", package = "PDRobust")
continuous_map <- Mapping(
  id = "patient_id",
  time = "visit_month",
  treatment = "treatment",
  survival = "alive_status",
  outcome = "clinical_outcome",
  baseline_time = 0,
  cutoff_time = 12,
  covariates = paste0("X", 1:6),
  interest_vars = c("X1", "X2"),
  y_type = "C"
)
continuous_data <- DataStandard(
  ImperfectConSample, continuous_map, drop = TRUE
)

continuous_sa <- SA(
  continuous_data,
  treatment ~ X1 + X2 + X4,
  alive_status ~ X1 + X2 + X4 + treatment + visit_month,
  clinical_outcome ~ X1 + X2 + treatment,
  ratiovec = c(0, 0.05)
)

binary_sa$data
continuous_sa$data
```

</div>

At each observed time, `SA()` defines perturbation variance using the
ordinary variance of all observed outcomes at that time. Binary analyses
retain a binomial logistic nuisance model and use the bounded-link HTE
equation; continuous analyses retain the original linear-model and
closed-form equations.

</div>

</div>

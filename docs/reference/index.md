<div id="main" class="col-md-9" role="main">

# Package index

<div class="section level2">

## Package overview

</div>

<div class="section level2">

-   `PDRobust` `PDRobust-package` : PDRobust: Principal-stratification
    treatment-effect estimation
-   `print(<pd_mapping>)` `print(<pd_data_check>)` `` `[`(<pd_data>) ``
    `print(<pd_hte_timevarying>)` `print(<pd_hte_pooled>)`
    `print(<PSDiag>)` `print(<PrinSDiag>)` `print(<odds_ratios>)`
    `print(<QR>)` `print(<SA>)` `plot(<pd_hte_timevarying>)`
    `plot(<pd_hte_pooled>)` `plot(<PSDiag>)` `plot(<PrinSDiag>)`
    `plot(<odds_ratios>)` : Display and subset PDRobust objects

</div>

<div class="section level2">

## Mapping and prepared data

<div class="section-desc">

Define the data-layout contract, validate raw data, and attach the
standardized mapping.

</div>

</div>

<div class="section level2">

-   `Mapping()` : Define the PDRobust data mapping
-   `DataCheck()` : Validate longitudinal principal-stratification data
-   `DataStandard()` : Standardize longitudinal principal-stratification
    data
-   `BiSample` : Binary longitudinal example data
-   `ImperfectConSample` : Imperfect Continuous Longitudinal Example
    Data

</div>

<div class="section level2">

## Independent prediction functions

<div class="section-desc">

Refit and predict independently on every call without cached fitted
models.

</div>

</div>

<div class="section level2">

-   `PSPred()` : Estimate propensity scores
-   `PrinPred()` : Estimate cumulative principal scores
-   `OutPred()` : Estimate outcome predictions

</div>

<div class="section level2">

## Heterogeneous treatment effects

</div>

<div class="section level2">

-   `HTESepT()` : Estimate time-specific heterogeneous treatment effects
-   `HTEAllT()` : Estimate pooled heterogeneous treatment effects across
    all times

</div>

<div class="section level2">

## Diagnostics and supporting analyses

</div>

<div class="section level2">

-   `PSDiag()` : Diagnose propensity-score covariate balance
-   `PrinSDiag()` : Diagnose principal-score balance
-   `QR()` : Summarize cutoff covariates in the always-survivor
    principal stratum
-   `ORCI()` : Estimate treatment-group-specific survival odds ratios at
    cutoff
-   `SA()` : Perform outcome-noise sensitivity analysis

</div>

</div>

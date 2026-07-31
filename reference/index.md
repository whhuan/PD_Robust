# Package index

## Package overview

- [`PDRobust`](https://whhuan.github.io/PD_Robust/reference/PDRobust-package.md)
  [`PDRobust-package`](https://whhuan.github.io/PD_Robust/reference/PDRobust-package.md)
  : PDRobust: Principal-stratification treatment-effect estimation

## Mapping and prepared data

Define the data-layout contract, validate raw data, and attach the
standardized mapping.

- [`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md)
  : Define the PDRobust data mapping
- [`DataCheck()`](https://whhuan.github.io/PD_Robust/reference/DataCheck.md)
  : Validate longitudinal principal-stratification data
- [`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md)
  : Standardize longitudinal principal-stratification data
- [`BiSample`](https://whhuan.github.io/PD_Robust/reference/BiSample.md)
  : Binary longitudinal example data
- [`ImperfectConSample`](https://whhuan.github.io/PD_Robust/reference/ImperfectConSample.md)
  : Imperfect Continuous Longitudinal Example Data

## Independent prediction functions

Refit and predict independently on every call without cached fitted
models.

- [`PSPred()`](https://whhuan.github.io/PD_Robust/reference/PSPred.md) :
  Estimate propensity scores
- [`PrinPred()`](https://whhuan.github.io/PD_Robust/reference/PrinPred.md)
  : Estimate cumulative principal scores
- [`OutPred()`](https://whhuan.github.io/PD_Robust/reference/OutPred.md)
  : Estimate outcome predictions

## Heterogeneous treatment effects

- [`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md)
  : Estimate time-specific heterogeneous treatment effects
- [`HTEAllT()`](https://whhuan.github.io/PD_Robust/reference/HTEAllT.md)
  : Estimate pooled heterogeneous treatment effects across all times

## Diagnostics and supporting analyses

- [`PSDiag()`](https://whhuan.github.io/PD_Robust/reference/PSDiag.md) :
  Diagnose propensity-score covariate balance
- [`PrinSDiag()`](https://whhuan.github.io/PD_Robust/reference/PrinSDiag.md)
  : Diagnose principal-score balance
- [`QR()`](https://whhuan.github.io/PD_Robust/reference/QR.md) :
  Summarize cutoff covariates in the always-survivor principal stratum
- [`ORCI()`](https://whhuan.github.io/PD_Robust/reference/ORCI.md) :
  Estimate treatment-group-specific survival odds ratios at cutoff
- [`SA()`](https://whhuan.github.io/PD_Robust/reference/SA.md) : Perform
  outcome-noise sensitivity analysis

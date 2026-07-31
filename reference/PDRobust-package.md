# PDRobust: Principal-stratification treatment-effect estimation

PDRobust provides principal-stratification methods for longitudinal
outcomes truncated by death.
[`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md)
defines structural columns, baseline and cutoff times, prediction-model
covariates, effect modifiers, and outcome type.
[`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md)
attaches the standardized mapping as the sole source of downstream
data-layout information.

## Details

[`PSPred()`](https://whhuan.github.io/PD_Robust/reference/PSPred.md),
[`PrinPred()`](https://whhuan.github.io/PD_Robust/reference/PrinPred.md),
and
[`OutPred()`](https://whhuan.github.io/PD_Robust/reference/OutPred.md)
each fit and predict internally on every call. They return pure numeric
vectors of class `pd_prediction`; fitted models are neither returned nor
cached.
[`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md)
alone accepts `target_time`, whereas
[`HTEAllT()`](https://whhuan.github.io/PD_Robust/reference/HTEAllT.md)
always analyzes every observed time from baseline through cutoff. No
hidden global or session cache is created.

## See also

Useful links:

- <https://github.com/whhuan/PD_Robust>
  <https://whhuan.github.io/PD_Robust/>

- Report bugs at <https://github.com/whhuan/PD_Robust/issues>

## Author

**Maintainer**: Huan Wang <whhuan42@gmail.com>

Authors:

- Huan Wang <whhuan42@gmail.com>

- Yilin Zhang

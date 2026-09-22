# PDRobust: Principal-stratification treatment-effect estimation

PDRobust estimates treatment effects for longitudinal outcomes that may
be unavailable after death. The effects apply to the always-survivor
principal stratum: subjects who would survive through the selected
cutoff time under either treatment.
[`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md)
identifies the variables and analysis times, and
[`DataStandard()`](https://whhuan.github.io/PD_Robust/reference/DataStandard.md)
prepares the data for the remaining functions.

## Details

[`PSPred()`](https://whhuan.github.io/PD_Robust/reference/PSPred.md),
[`PrinPred()`](https://whhuan.github.io/PD_Robust/reference/PrinPred.md),
and
[`OutPred()`](https://whhuan.github.io/PD_Robust/reference/OutPred.md)
fit their models each time they are called and return numeric
predictions.
[`HTESepT()`](https://whhuan.github.io/PD_Robust/reference/HTESepT.md)
estimates effects separately at user-selected times, whereas
[`HTEAllT()`](https://whhuan.github.io/PD_Robust/reference/HTEAllT.md)
estimates a joint trajectory over every observed time from baseline
through cutoff.

## Treatment coding

The implemented estimator uses treatment `1` as the survival-favorable
arm: potential survival satisfies \\S^1 \ge S^0\\ at cutoff. Its
always-survivor principal score is therefore the survival probability
under treatment `0`. If the survival-favorable arm is coded as `0` in
the raw data, recode the raw treatment as `1 - A` before mapping and
standardizing. To report the original contrast, negate the package
estimate and transform an interval `[lower, upper]` to
`[-upper, -lower]`.
[`Mapping()`](https://whhuan.github.io/PD_Robust/reference/Mapping.md)
does not infer or reverse treatment coding.

## Interpretation and assumptions

The target population comprises subjects who would survive through the
selected cutoff under either treatment. The effect model describes
outcome differences at earlier analysis times within that fixed
population. Continuous-outcome models describe effects on a linear
scale. Binary-outcome models transform the model's linear predictor with
`2 * plogis(eta) - 1`; their coefficients are not odds ratios.

Causal interpretation requires consistency, no interference, adequate
treatment and survival overlap, treatment ignorability conditional on
the measured covariates, the stated survival monotonicity, and principal
ignorability. Checking the data and covariate balance cannot establish
these assumptions. Triple robustness means that, under the method's
assumptions and regularity conditions, at least two of the propensity
score, principal score, and outcome mean models must be correctly
specified. It does not guarantee unbiased results in every finite
sample. Limiting extreme probabilities can also affect the estimates.

[`SA()`](https://whhuan.github.io/PD_Robust/reference/SA.md) examines
sensitivity by adding random noise to the outcome. It does not vary the
principal-ignorability assumption. See the `method-and-coding` vignette
for treatment coding and other implementation limits.

## References

Zhang, Y., Shardell, M., Falvey, J., McCoy, R., Stuart, E., and Chen, C.
(2026). A Novel Tool for Evaluating Effect Modification in Older Adults
with ADRD Using Medicare Claims. arXiv:2608.06654.
[doi:10.48550/arXiv.2608.06654](https://doi.org/10.48550/arXiv.2608.06654)
.

## See also

Useful links:

- <https://github.com/whhuan/PD_Robust>

- <https://whhuan.github.io/PD_Robust/>

- Report bugs at <https://github.com/whhuan/PD_Robust/issues>

## Author

**Maintainer**: Huan Wang <whhuan42@gmail.com>

Authors:

- Huan Wang <whhuan42@gmail.com>

- Yilin Zhang

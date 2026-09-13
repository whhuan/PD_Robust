<div id="main" class="col-md-9" role="main">

# PDRobust: Principal-stratification treatment-effect estimation

<div class="ref-description section level2">

PDRobust provides principal-stratification methods for longitudinal
outcomes truncated by death. `Mapping()` defines structural columns,
baseline and cutoff times, prediction-model covariates, effect
modifiers, and outcome type. `DataStandard()` attaches the standardized
mapping as the sole source of downstream data-layout information.

</div>

<div class="section level2">

## Details

`PSPred()`, `PrinPred()`, and `OutPred()` each fit and predict
internally on every call. They return pure numeric vectors of class
`pd_prediction`; fitted models are neither returned nor cached.
`HTESepT()` alone accepts `target_time`, whereas `HTEAllT()` always
analyzes every observed time from baseline through cutoff. No hidden
global or session cache is created.

</div>

<div class="section level2">

## Treatment coding

The implemented estimator uses treatment `1` as the survival-favorable
arm: potential survival satisfies \\(S^1 \\ge S^0\\) at cutoff. Its
always-survivor principal score is therefore the survival probability
under treatment `0`. This convention is retained from version 0.3.7. The
main estimator in the reference below uses the opposite arm labels. To
analyze data coded in that convention, recode the raw treatment as
`1 - A` before mapping and standardizing. To report the original
contrast, negate the package estimate and transform an interval
`[lower, upper]` to `[-upper, -lower]`. `Mapping()` does not infer or
reverse treatment coding.

</div>

<div class="section level2">

## Interpretation and assumptions

The target population comprises subjects who would survive through the
selected cutoff under either treatment. The effect model describes
outcome differences at earlier analysis times within that fixed
population. Continuous-outcome models use a linear effect function.
Binary-outcome models use `2 * plogis(eta) - 1`, where `eta` is the
linear predictor; coefficients are on that link scale, not the
odds-ratio scale.

Causal interpretation requires consistency, no interference, adequate
treatment and survival overlap, exposure ignorability conditional on the
measured covariates, the stated survival monotonicity, and principal
ignorability. Data validation and covariate balance do not establish
these assumptions. Triple robustness concerns correct specification of
at least two of the three nuisance-model components under the method's
assumptions and regularity conditions; it is not an unconditional
finite-sample guarantee. Fixed probability clipping can alter the
estimating equations.

`SA()` implements random outcome-noise perturbation. It does not
implement the reference's principal-ignorability sensitivity parameter.
See the `method-and-coding` vignette for the coding conversion and
implementation limits.

</div>

<div class="section level2">

## References

Zhang, Y., Shardell, M., Falvey, J., McCoy, R., Stuart, E., and Chen, C.
(2026). A Novel Tool for Evaluating Effect Modification in Older Adults
with ADRD Using Medicare Claims. arXiv:2608.06654.
[doi:10.48550/arXiv.2608.06654](https://doi.org/10.48550/arXiv.2608.06654)
.

</div>

<div class="section level2">

## See also

<div class="dont-index">

Useful links:

-   <https://github.com/whhuan/PD_Robust>

-   <https://whhuan.github.io/PD_Robust/>

-   Report bugs at <https://github.com/whhuan/PD_Robust/issues>

</div>

</div>

<div class="section level2">

## Author

**Maintainer**: Huan Wang <whhuan42@gmail.com>

Authors:

-   Huan Wang <whhuan42@gmail.com>

-   Yilin Zhang

</div>

</div>

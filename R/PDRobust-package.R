#' PDRobust: Principal-stratification treatment-effect estimation
#'
#' PDRobust provides principal-stratification methods for longitudinal outcomes
#' truncated by death. `Mapping()` defines structural columns, baseline and
#' cutoff times, prediction-model covariates, effect modifiers, and outcome
#' type. `DataStandard()` attaches the standardized mapping as the sole source
#' of downstream data-layout information.
#'
#' `PSPred()`, `PrinPred()`, and `OutPred()` each fit and predict internally on
#' every call. They return pure numeric vectors of class `pd_prediction`; fitted
#' models are neither returned nor cached. `HTESepT()` alone accepts
#' `target_time`, whereas `HTEAllT()` always analyzes every observed time from
#' baseline through cutoff. No hidden global or session cache is created.
#'
#' @section Treatment coding:
#' The implemented estimator uses treatment `1` as the survival-favorable arm:
#' potential survival satisfies \eqn{S^1 \ge S^0} at cutoff. Its always-survivor
#' principal score is therefore the survival probability under treatment `0`.
#' If the survival-favorable arm is coded as `0` in the raw data,
#' recode the raw treatment as `1 - A` before mapping and
#' standardizing. To report the original contrast, negate the package estimate
#' and transform an interval `[lower, upper]` to `[-upper, -lower]`.
#' `Mapping()` does not infer or reverse treatment coding.
#'
#' @section Interpretation and assumptions:
#' The target population comprises subjects who would survive through the
#' selected cutoff under either treatment. The effect model describes outcome
#' differences at earlier analysis times within that fixed population.
#' Continuous-outcome models use a linear effect function. Binary-outcome
#' models use `2 * plogis(eta) - 1`, where `eta` is the linear predictor;
#' coefficients are on that link scale, not the odds-ratio scale.
#'
#' Causal interpretation requires consistency, no interference, adequate
#' treatment and survival overlap, exposure ignorability conditional on the
#' measured covariates, the stated survival monotonicity, and principal
#' ignorability. Data validation and covariate balance do not establish these
#' assumptions. Triple robustness concerns correct specification of at least
#' two of the three nuisance-model components under the method's assumptions
#' and regularity conditions; it is not an unconditional finite-sample
#' guarantee. Fixed probability clipping can alter the estimating equations.
#'
#' `SA()` implements random outcome-noise perturbation. It does not implement
#' the reference's principal-ignorability sensitivity parameter. See the
#' `method-and-coding` vignette for the coding conversion and implementation
#' limits.
#'
#' @references
#' Zhang, Y., Shardell, M., Falvey, J., McCoy, R., Stuart, E., and Chen, C.
#' (2026). A Novel Tool for Evaluating Effect Modification in Older Adults
#' with ADRD Using Medicare Claims. arXiv:2608.06654.
#' \doi{10.48550/arXiv.2608.06654}.
#'
#' @importFrom utils globalVariables
#' @keywords internal
"_PACKAGE"

globalVariables(c(
  "estimate", "time", "covariate", "term", "adjustment", "smd",
  "statistic", "covname", "estcoef", "lowerbd", "upperbd", "ratio",
  "LowerBound", "UpperBound",".pd_weights"
))

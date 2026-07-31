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
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  "estimate", "time", "covariate", "term", "adjustment", "smd",
  "statistic", "covname", "estcoef", "lowerbd", "upperbd", "ratio",
  "LowerBound", "UpperBound",".pd_weights"
))

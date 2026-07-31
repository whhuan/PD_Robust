#' Estimate propensity scores
#'
#' Fits a logistic propensity-score model on baseline observations from
#' `fit_dat` and predicts on every row of `pred_dat`. The model is refitted on
#' every call; no fitted object or cache is retained.
#'
#' @param ps_fo Propensity-score formula.
#' @param fit_dat Data used to fit the model.
#' @param pred_dat Data on which to predict.
#' @param mapping A `pd_mapping` object. It supplies column names and
#'   `baseline_time` only; it never supplies data.
#' @param ... Additional arguments passed to `stats::glm()`.
#'
#' @return A numeric vector of class `pd_prediction` with length
#'   `nrow(pred_dat)`, rounded to three decimal places after prediction.
#' @examples
#' data("BiSample", package = "PDRobust")
#' map <- Mapping(
#'   id = "id", time = "time", treatment = "A",
#'   survival = "S", outcome = "Y",
#'   baseline_time = 0, cutoff_time = 2,
#'   covariates = c("X1", "X2", "X4"),
#'   interest_vars = c("X1", "X2"), y_type = "B"
#' )
#' pd_dat <- DataStandard(BiSample, map)
#' ps <- PSPred(A ~ X1 + X2 + X4, pd_dat, pd_dat, map)
#' head(ps)
#' @export
PSPred <- function(ps_fo, fit_dat, pred_dat, mapping, ...) {
  .pd_round_prediction(
    .pd_pspred_impl(ps_fo, fit_dat, pred_dat, mapping, ...)
  )
}

#' Full-precision propensity-score implementation
#'
#' @noRd
.pd_pspred_impl <- function(ps_fo, fit_dat, pred_dat, mapping, ...) {
  mapping <- .pd_validate_mapping(mapping)
  fit_dat <- .pd_as_data_frame(fit_dat)
  pred_dat <- .pd_as_data_frame(pred_dat)
  .pd_assert_nonempty(fit_dat)
  .pd_assert_nonempty(pred_dat)
  .pd_assert_columns(
    fit_dat, c(mapping$id_col, mapping$time_col, mapping$A_col)
  )
  ps_fo <- .pd_validate_formula(ps_fo, fit_dat, "ps_fo")
  .pd_assert_columns(
    pred_dat, .pd_formula_variables(ps_fo)
  )

  baseline <- fit_dat[
    fit_dat[[mapping$time_col]] == mapping$baseline_time,
    , drop = FALSE
  ]
  if (!nrow(baseline)) {
    .pd_stop("No observations were found at `mapping$baseline_time`.")
  }
  if (anyNA(baseline[[mapping$id_col]]) ||
      anyDuplicated(.pd_key(baseline[[mapping$id_col]]))) {
    .pd_stop("`fit_dat` must contain exactly one baseline row per subject.")
  }
  variables <- .pd_formula_variables(ps_fo)
  if (!identical(variables[1L], mapping$A_col)) {
    .pd_stop("`ps_fo` must use the mapped treatment column as its response.")
  }
  baseline <- baseline[
    stats::complete.cases(baseline[, variables, drop = FALSE]),
    , drop = FALSE
  ]
  if (!nrow(baseline)) {
    .pd_stop("No complete baseline observations are available for `PSPred()`.")
  }

  response <- variables[1L]
  observed <- unique(baseline[[response]])
  if (!all(observed %in% c(0, 1)) || length(observed) < 2L) {
    .pd_stop("The propensity-score response must contain both 0 and 1 at baseline.")
  }

  fit <- .pd_fit_glm_checked(
    ps_fo, baseline,
    label = "PSPred propensity-score model",
    allow_aliased = TRUE,
    strict = FALSE,
    diagnostic_context = list(
      n_subjects = length(unique(.pd_key(baseline[[mapping$id_col]])))
    ),
    ...
  )
  prediction <- .pd_predict_checked(
    fit, pred_dat, "PSPred propensity-score model",
    allow_rank_deficient = TRUE
  )
  out <- .pd_prediction(prediction)
  attr(out, "pd_model_diagnostics") <-
    attr(fit, "pd_model_diagnostics", exact = TRUE)
  out
}

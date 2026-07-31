#' Estimate cumulative principal scores
#'
#' Fits the principal-score model and returns cumulative survival probabilities
#' for all rows of `pred_dat`. All actual observed times from baseline through
#' cutoff are used. The model is refitted on every call.
#'
#' When multiple observed time points exist, baseline rows are assigned an
#' at-risk indicator of zero and each post-baseline row is included only when
#' the subject survived at the immediately preceding observed time. When the
#' analysis contains only one observed time point, no at-risk indicator is
#' constructed and all complete observations at that time are used for fitting.
#'
#' @param prin_fo Principal-score formula.
#' @param fit_dat Data used to fit the model.
#' @param pred_dat Data on which to predict cumulative scores.
#' @param a Treatment level for principal-score prediction, either `0` or `1`.
#' @param mapping A `pd_mapping` object.
#' @param ... Additional arguments passed to `stats::glm()`.
#'
#' @return A numeric vector of class `pd_prediction` with length
#'   `nrow(pred_dat)`, rounded to three decimal places after cumulative
#'   probabilities have been calculated.
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
#' score0 <- PrinPred(
#'   S ~ X1 + X2 + X4 + A + time,
#'   pd_dat, pd_dat, a = 0, mapping = map
#' )
#' head(score0)
#' @export
PrinPred <- function(prin_fo, fit_dat, pred_dat, a, mapping, ...) {
  .pd_round_prediction(
    .pd_prinpred_impl(
      prin_fo, fit_dat, pred_dat, a, mapping, ...
    )
  )
}

#' Full-precision principal-score implementation
#'
#' @noRd
.pd_prinpred_impl <- function(prin_fo, fit_dat, pred_dat, a,
                              mapping, ...) {
  if (length(a) != 1L || is.na(a) || !a %in% c(0, 1)) {
    .pd_stop("`a` must be 0 or 1.")
  }
  prepared <- .pd_prinpred_fit(
    prin_fo = prin_fo,
    fit_dat = fit_dat,
    pred_dat = pred_dat,
    mapping = mapping,
    diagnostic_treatment = a,
    ...
  )
  .pd_prinpred_predict(prepared, a)
}

#' Fit one full-precision principal-score model for internal prediction
#'
#' @noRd
.pd_prinpred_fit <- function(prin_fo, fit_dat, pred_dat, mapping,
                             diagnostic_treatment = NA_real_, ...) {
  mapping <- .pd_validate_mapping(mapping)
  fit_dat <- .pd_as_data_frame(fit_dat)
  pred_dat <- .pd_as_data_frame(pred_dat)
  .pd_assert_nonempty(fit_dat)
  .pd_assert_nonempty(pred_dat)
  structural <- c(
    mapping$id_col, mapping$time_col, mapping$A_col, mapping$S_col
  )
  .pd_assert_columns(fit_dat, structural)
  .pd_assert_columns(pred_dat, structural)
  prin_fo <- .pd_validate_formula(prin_fo, fit_dat, "prin_fo")
  .pd_assert_columns(pred_dat, .pd_formula_variables(prin_fo))
  if (!identical(.pd_formula_variables(prin_fo)[1L], mapping$S_col)) {
    .pd_stop("`prin_fo` must use the mapped survival column as its response.")
  }

  in_fit_window <- fit_dat[[mapping$time_col]] >= mapping$baseline_time &
    fit_dat[[mapping$time_col]] <= mapping$cutoff_time
  if (anyNA(in_fit_window)) {
    .pd_stop("`fit_dat` contains missing time values.")
  }
  fit_dat <- fit_dat[in_fit_window, , drop = FALSE]
  fit_pair <- paste(
    .pd_key(fit_dat[[mapping$id_col]]),
    fit_dat[[mapping$time_col]], sep = "\r"
  )
  if (anyDuplicated(fit_pair)) {
    .pd_stop("`fit_dat` must contain at most one row per subject and time.")
  }
  fit_times <- sort(unique(stats::na.omit(
    as.numeric(fit_dat[[mapping$time_col]])
  )))
  if (!length(fit_times) || !mapping$baseline_time %in% fit_times ||
      !mapping$cutoff_time %in% fit_times) {
    .pd_stop("`fit_dat` must contain the mapped baseline and cutoff times.")
  }

  in_pred_window <- pred_dat[[mapping$time_col]] >= mapping$baseline_time &
    pred_dat[[mapping$time_col]] <= mapping$cutoff_time
  if (anyNA(in_pred_window) || !all(in_pred_window)) {
    .pd_stop("`pred_dat` contains missing times or times outside baseline through cutoff.")
  }
  pred_times <- sort(unique(as.numeric(pred_dat[[mapping$time_col]])))
  if (!isTRUE(all.equal(pred_times, fit_times))) {
    .pd_stop(
      "`pred_dat` must contain every actual observed time in `fit_dat` from baseline through cutoff."
    )
  }
  pred_pair <- paste(
    .pd_key(pred_dat[[mapping$id_col]]),
    pred_dat[[mapping$time_col]], sep = "\r"
  )
  if (anyDuplicated(pred_pair)) {
    .pd_stop("`pred_dat` must contain at most one row per subject and time.")
  }
  pred_groups <- split(
    seq_len(nrow(pred_dat)), .pd_key(pred_dat[[mapping$id_col]])
  )
  complete_prediction_panel <- vapply(pred_groups, function(idx) {
    observed <- sort(unique(as.numeric(pred_dat[[mapping$time_col]][idx])))
    isTRUE(all.equal(observed, fit_times))
  }, logical(1))
  if (!all(complete_prediction_panel)) {
    .pd_stop(
      "Every subject in `pred_dat` must have one row at every actual observed time from baseline through cutoff."
    )
  }

  fit_order <- order(
    fit_dat[[mapping$id_col]], fit_dat[[mapping$time_col]], na.last = TRUE
  )
  fit_dat <- fit_dat[fit_order, , drop = FALSE]
  post_times <- sort(unique(fit_dat[[mapping$time_col]][
    fit_dat[[mapping$time_col]] > mapping$baseline_time
  ]))

  if (length(fit_times) == 1L) {
    # Single-time analysis: use all observations and do not construct `ind`.
    fit_rows <- rep(TRUE, nrow(fit_dat))
  } else {
    # Longitudinal analysis: baseline is not in the risk set. Each subsequent
    # row is at risk only when survival at the immediately preceding time is 1.
    ind <- integer(nrow(fit_dat))
    groups <- split(
      seq_len(nrow(fit_dat)), .pd_key(fit_dat[[mapping$id_col]])
    )
    for (idx in groups) {
      idx <- idx[order(fit_dat[[mapping$time_col]][idx])]
      ind[idx[1L]] <- 0L
      previous <- fit_dat[[mapping$S_col]][idx[-length(idx)]]
      ind[idx[-1L]] <- as.integer(!is.na(previous) & previous == 1)
    }
    fit_dat$ind <- ind
    fit_rows <- fit_dat$ind == 1L
  }

  risk_data <- fit_dat[fit_rows, , drop = FALSE]
  if (!nrow(risk_data)) {
    .pd_stop("No at-risk observations are available for `PrinPred()`.")
  }
  fitting_formula <- .pd_exclude_design_fixed_predictors(
    prin_fo,
    risk_data,
    fixed = mapping$A_col,
    label = "PrinPred principal-score model"
  )
  fitting_variables <- .pd_formula_variables(fitting_formula)
  model_data <- risk_data[
    stats::complete.cases(risk_data[, fitting_variables, drop = FALSE]),
    , drop = FALSE
  ]
  if (!nrow(model_data)) {
    .pd_stop("No at-risk complete observations are available for `PrinPred()`.")
  }

  response <- fitting_variables[1L]
  observed <- unique(model_data[[response]])
  context_warnings <- if (
    !all(observed %in% c(0, 1)) || length(observed) < 2L
  ) {
    paste0(
      "the principal-score response has only one observed level among fitted ",
      "at-risk rows; finite constant predictions will be retained if available"
    )
  } else {
    character()
  }
  fit <- .pd_fit_glm_checked(
    fitting_formula,
    model_data,
    label = "PrinPred principal-score model",
    allow_aliased = TRUE,
    strict = FALSE,
    context_warnings = context_warnings,
    diagnostic_context = list(
      treatment = diagnostic_treatment,
      n_subjects = length(unique(.pd_key(model_data[[mapping$id_col]])))
    ),
    ...
  )

  original_order <- seq_len(nrow(pred_dat))
  pred_dat$.pd_original_order <- original_order
  pred_dat <- pred_dat[order(
    pred_dat[[mapping$id_col]], pred_dat[[mapping$time_col]], na.last = TRUE
  ), , drop = FALSE]

  list(
    fit = fit,
    pred_dat = pred_dat,
    post_times = post_times,
    mapping = mapping
  )
}

#' Predict one treatment arm from a fitted principal-score model
#'
#' @noRd
.pd_prinpred_predict <- function(prepared, a) {
  if (length(a) != 1L || is.na(a) || !a %in% c(0, 1)) {
    .pd_stop("`a` must be 0 or 1.")
  }
  fit <- prepared$fit
  pred_dat <- prepared$pred_dat
  post_times <- prepared$post_times
  mapping <- prepared$mapping
  pred_dat[[mapping$A_col]] <- as.numeric(a)
  conditional <- .pd_predict_checked(
    fit, pred_dat, "PrinPred principal-score model",
    allow_rank_deficient = TRUE
  )
  if (length(post_times)) {
    conditional[pred_dat[[mapping$time_col]] == mapping$baseline_time] <- 1
  }

  cumulative <- numeric(nrow(pred_dat))
  groups <- split(seq_len(nrow(pred_dat)), .pd_key(pred_dat[[mapping$id_col]]))
  for (idx in groups) {
    idx <- idx[order(pred_dat[[mapping$time_col]][idx])]
    cumulative[idx] <- cumprod(conditional[idx])
  }
  cumulative <- cumulative[order(pred_dat$.pd_original_order)]
  out <- .pd_prediction(cumulative)
  attr(out, "pd_model_diagnostics") <-
    attr(fit, "pd_model_diagnostics", exact = TRUE)
  out
}

#' Predict both treatment arms from one internal principal-score fit
#'
#' The fitted model does not depend on the counterfactual treatment value used
#' for prediction. Internal analyses therefore fit once and predict twice,
#' while each public `PrinPred()` call continues to fit independently.
#'
#' @noRd
.pd_prinpred_pair_impl <- function(prin_fo, fit_dat, pred_dat, mapping, ...) {
  prepared <- .pd_prinpred_fit(
    prin_fo = prin_fo,
    fit_dat = fit_dat,
    pred_dat = pred_dat,
    mapping = mapping,
    diagnostic_treatment = NA_real_,
    ...
  )
  list(
    p0 = .pd_prinpred_predict(prepared, 0),
    p1 = .pd_prinpred_predict(prepared, 1),
    model_diagnostics = attr(
      prepared$fit, "pd_model_diagnostics", exact = TRUE
    )
  )
}

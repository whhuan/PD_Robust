#' Estimate outcome predictions
#'
#' Refits the outcome model on every call and predicts on all rows of
#' `pred_dat`. During prediction the mapped treatment column is set to `a` and
#' the mapped survival column is set to one, exactly as in the original method.
#'
#' @param out_fo Outcome-model formula.
#' @param fit_dat Data used to fit the outcome model.
#' @param pred_dat Data on which to predict.
#' @param a Treatment value, either `0` or `1`.
#' @param mapping A `pd_mapping` object. `mapping$y_type` selects linear or
#'   logistic regression.
#' @param ... Additional arguments passed to `stats::lm()` or `stats::glm()`.
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
#' mu1 <- OutPred(Y ~ X1 + X2 + A + S, pd_dat, pd_dat, a = 1, mapping = map)
#' head(mu1)
#' @export
OutPred <- function(out_fo, fit_dat, pred_dat, a, mapping, ...) {
  .pd_round_prediction(
    .pd_outpred_impl(out_fo, fit_dat, pred_dat, a, mapping, ...)
  )
}

#' Full-precision outcome-prediction implementation
#'
#' @noRd
.pd_outpred_impl <- function(out_fo, fit_dat, pred_dat, a, mapping, ...) {
  if (length(a) != 1L || is.na(a) || !a %in% c(0, 1)) {
    .pd_stop("`a` must be 0 or 1.")
  }
  prepared <- .pd_outpred_fit(
    out_fo = out_fo,
    fit_dat = fit_dat,
    pred_dat = pred_dat,
    mapping = mapping,
    diagnostic_treatment = a,
    ...
  )
  .pd_outpred_predict(prepared, a)
}

#' Fit one full-precision outcome model for internal prediction
#'
#' @noRd
.pd_outpred_fit <- function(out_fo, fit_dat, pred_dat, mapping,
                            diagnostic_treatment = NA_real_, ...) {
  mapping <- .pd_validate_mapping(mapping)
  fit_dat <- .pd_as_data_frame(fit_dat)
  pred_dat <- .pd_as_data_frame(pred_dat)
  .pd_assert_nonempty(fit_dat)
  .pd_assert_nonempty(pred_dat)
  .pd_assert_columns(pred_dat, c(mapping$A_col, mapping$S_col))
  out_fo <- .pd_validate_formula(out_fo, fit_dat, "out_fo")
  .pd_assert_columns(pred_dat, .pd_formula_variables(out_fo))

  response <- .pd_formula_variables(out_fo)[1L]
  if (!identical(response, mapping$Y_col)) {
    .pd_stop("`out_fo` must use the mapped outcome column as its response.")
  }
  # Match the original MuPred() rule: remove every fitting row whose
  # response is NA, regardless of survival status or the missingness reason.
  fit_dat <- fit_dat[!is.na(fit_dat[[response]]), , drop = FALSE]
  if (!nrow(fit_dat)) {
    .pd_stop("No nonmissing outcomes are available for `OutPred()`.")
  }

  if (identical(mapping$y_type, "B")) {
    observed <- unique(fit_dat[[response]])
    context_warnings <- if (
      !all(observed %in% c(0, 1)) || length(observed) < 2L
    ) {
      paste0(
        "the binary outcome response has only one observed level; finite ",
        "constant predictions will be retained if available"
      )
    } else {
      character()
    }
    fit <- .pd_fit_glm_checked(
      out_fo,
      fit_dat,
      label = "OutPred binary-outcome model",
      allow_aliased = TRUE,
      strict = FALSE,
      context_warnings = context_warnings,
      diagnostic_context = list(
        treatment = diagnostic_treatment,
        n_subjects = if (mapping$id_col %in% names(fit_dat)) {
          length(unique(.pd_key(fit_dat[[mapping$id_col]])))
        } else {
          NA_integer_
        }
      ),
      ...
    )
  } else {
    fit <- .pd_fit_lm_checked(
      out_fo,
      fit_dat,
      label = "OutPred continuous-outcome model",
      allow_aliased = TRUE,
      strict = FALSE,
      diagnostic_context = list(
        treatment = diagnostic_treatment,
        n_subjects = if (mapping$id_col %in% names(fit_dat)) {
          length(unique(.pd_key(fit_dat[[mapping$id_col]])))
        } else {
          NA_integer_
        }
      ),
      ...
    )
  }

  list(
    fit = fit,
    pred_dat = pred_dat,
    mapping = mapping
  )
}

#' Predict one treatment arm from a fitted outcome model
#'
#' @noRd
.pd_outpred_predict <- function(prepared, a) {
  if (length(a) != 1L || is.na(a) || !a %in% c(0, 1)) {
    .pd_stop("`a` must be 0 or 1.")
  }
  fit <- prepared$fit
  prediction_data <- prepared$pred_dat
  mapping <- prepared$mapping
  prediction_data[[mapping$A_col]] <- as.numeric(a)
  prediction_data[[mapping$S_col]] <- 1
  prediction <- .pd_predict_checked(
    fit,
    prediction_data,
    "OutPred outcome model",
    allow_rank_deficient = TRUE
  )
  out <- .pd_prediction(prediction)
  attr(out, "pd_model_diagnostics") <-
    attr(fit, "pd_model_diagnostics", exact = TRUE)
  out
}

#' Predict both treatment arms from one internal outcome-model fit
#'
#' @noRd
.pd_outpred_pair_impl <- function(out_fo, fit_dat, pred_dat, mapping, ...) {
  prepared <- .pd_outpred_fit(
    out_fo = out_fo,
    fit_dat = fit_dat,
    pred_dat = pred_dat,
    mapping = mapping,
    diagnostic_treatment = NA_real_,
    ...
  )
  list(
    mu0 = .pd_outpred_predict(prepared, 0),
    mu1 = .pd_outpred_predict(prepared, 1),
    model_diagnostics = attr(
      prepared$fit, "pd_model_diagnostics", exact = TRUE
    )
  )
}

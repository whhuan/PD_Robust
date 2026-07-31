# Public workflow ----------------------------------------------------------

#' @noRd
.pd_require_prepared_data <- function(data, caller) {
  if (!inherits(data, "pd_data")) {
    .pd_stop(
      "`data` supplied to `", caller,
      "()` must be returned by `DataStandard()`."
    )
  }
  check <- attr(data, "pd_check", exact = TRUE)
  if (!inherits(check, "pd_data_check") ||
      !isTRUE(check$ready_for_analysis)) {
    .pd_stop(
      "`data` supplied to `", caller,
      "()` is not marked ready for analysis. Review `attr(data, \"pd_check\")`."
    )
  }
  mapping <- .pd_mapping_for_data(data, caller)
  invisible(mapping)
}

#' @noRd
.pd_validate_interest_vars <- function(data, caller, mapping = NULL) {
  mapping <- mapping %||% .pd_mapping_for_data(data, caller)
  interest_vars <- mapping$interest_vars
  .pd_assert_columns(data, interest_vars)
  nonnumeric <- interest_vars[
    !vapply(data[interest_vars], is.numeric, logical(1))
  ]
  if (length(nonnumeric)) {
    .pd_stop(
      "`mapping$interest_vars` must be numeric for the current estimators: ",
      paste(nonnumeric, collapse = ", "), "."
    )
  }
  interest_vars
}

#' @noRd
.pd_validate_model_covariates <- function(ps_fo, prin_fo, out_fo, mapping) {
  formula_variables <- unique(unlist(lapply(
    list(ps_fo, prin_fo, out_fo),
    function(formula) {
      all.vars(stats::delete.response(stats::terms(formula)))
    }
  )))
  used <- setdiff(
    formula_variables,
    c(
      mapping$id_col, mapping$time_col, mapping$A_col,
      mapping$S_col, mapping$Y_col
    )
  )
  missing_from_mapping <- setdiff(used, mapping$covariates)
  if (length(missing_from_mapping)) {
    .pd_stop(
      "All non-structural prediction-model variables must be listed in ",
      "`mapping$covariates`; missing: ",
      paste(missing_from_mapping, collapse = ", "), "."
    )
  }
  invisible(TRUE)
}

#' @noRd
.pd_formula_covariates <- function(formula, mapping, label) {
  variables <- all.vars(stats::delete.response(stats::terms(formula)))
  variables <- setdiff(
    variables,
    c(
      mapping$id_col, mapping$time_col, mapping$A_col,
      mapping$S_col, mapping$Y_col
    )
  )
  variables <- unique(variables)
  if (!length(variables)) {
    .pd_stop("No analysis covariates were found in the ", label, " formula.")
  }
  variables
}

#' @noRd
.pd_analysis_times <- function(data, mapping = NULL) {
  mapping <- mapping %||% .pd_mapping_for_data(data, "time-range resolution")
  data <- .pd_as_data_frame(data)
  times <- sort(unique(stats::na.omit(as.numeric(data[[mapping$time_col]]))))
  times <- times[
    times >= mapping$baseline_time & times <= mapping$cutoff_time
  ]
  if (!length(times) || !mapping$baseline_time %in% times ||
      !mapping$cutoff_time %in% times) {
    .pd_stop("The data must contain the mapped baseline and cutoff times.")
  }
  expected <- seq.int(
    from = as.integer(mapping$baseline_time),
    to = as.integer(mapping$cutoff_time)
  )
  if (!isTRUE(all.equal(times, as.numeric(expected)))) {
    .pd_stop(
      "Prepared data must retain every standardized observed time from baseline through cutoff."
    )
  }
  times
}

#' @noRd
.pd_validate_target_time <- function(target_time, data, mapping) {
  if (!is.numeric(target_time) || !length(target_time) ||
      anyNA(target_time) || any(!is.finite(target_time))) {
    .pd_stop("`target_time` must be a non-empty finite numeric vector.")
  }
  target_time <- sort(unique(as.numeric(target_time)))
  available <- .pd_analysis_times(data, mapping)
  invalid <- setdiff(target_time, available)
  if (length(invalid)) {
    .pd_stop(
      "Every `target_time` value must be an observed standardized analysis time. ",
      "Invalid values: ", paste(.pd_time_label(invalid), collapse = ", "), "."
    )
  }
  target_time
}

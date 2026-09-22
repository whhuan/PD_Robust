#' Identify variables and analysis times for PDRobust
#'
#' Records which columns contain the subject ID, time, treatment, survival,
#' outcome, and covariates, together with the analysis time range and outcome
#' type. Other package functions use this information to interpret the data
#' consistently.
#'
#' All ten arguments are required. `target_time` is specified separately when
#' calling `HTESepT()` because it selects time points for that analysis rather
#' than describing the data.
#'
#' @param id A single character string naming the subject ID column.
#' @param time A single character string naming the time column.
#' @param treatment A single character string naming the treatment column. For causal
#'   estimation, code the survival-favorable arm as `1` and the other arm as
#'   `0`; see [PDRobust-package] for the convention and assumptions. The
#'   function does not determine which arm is survival-favorable.
#' @param survival A single character string naming the column that records
#'   survival or another intermediate status.
#' @param outcome A single character string naming the outcome column.
#' @param baseline_time A single finite number giving the baseline time in the
#'   original time scale.
#' @param cutoff_time The time point, on the original time scale, at which the
#'   always-survivor principal stratum is defined.
#' @param covariates A character vector naming all variables used as predictors
#'   in the propensity score, principal score, or outcome mean models.
#' @param interest_vars A character vector specifying the names of variables
#'   used to evaluate heterogeneous treatment effects. Each variable must also
#'   be included in `covariates`.
#' @param y_type The outcome type: `"C"` for continuous or `"B"` for binary.
#'
#' @return A `pd_mapping` object that can be supplied to `DataCheck()` and
#'   `DataStandard()`.
#' @examples
#' map <- Mapping(
#'   id = "id", time = "time", treatment = "A",
#'   survival = "S", outcome = "Y",
#'   baseline_time = 3,
#'   cutoff_time = 9,
#'   covariates = c("X1", "X2", "X4"),
#'   interest_vars = c("X1", "X2"),
#'   y_type = "C"
#' )
#' map
#' @export
Mapping <- function(id, time, treatment,
                    survival, outcome,
                    baseline_time, cutoff_time,
                    covariates, interest_vars, y_type) {
  columns <- list(
    id_col = id,
    time_col = time,
    A_col = treatment,
    S_col = survival,
    Y_col = outcome
  )
  valid_columns <- vapply(columns, function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }, logical(1))
  if (!all(valid_columns)) {
    .pd_stop("Every structural mapping entry must be one non-empty column name.")
  }
  if (anyDuplicated(unlist(columns, use.names = FALSE))) {
    .pd_stop("Each structural analysis role must map to a different column.")
  }

  if (!is.character(covariates) || !length(covariates) || anyNA(covariates) ||
      any(!nzchar(covariates))) {
    .pd_stop("`covariates` must contain every prediction-model covariate.")
  }
  covariates <- unique(covariates)
  overlap <- intersect(covariates, unlist(columns, use.names = FALSE))
  if (length(overlap)) {
    .pd_stop(
      "Mapped covariates cannot duplicate structural columns: ",
      paste(overlap, collapse = ", "), "."
    )
  }

  if (!is.character(interest_vars) || !length(interest_vars) ||
      anyNA(interest_vars) || any(!nzchar(interest_vars))) {
    .pd_stop("`interest_vars` must contain at least one variable name.")
  }
  interest_vars <- unique(interest_vars)
  missing_interest <- setdiff(interest_vars, covariates)
  if (length(missing_interest)) {
    .pd_stop(
      "Every `interest_vars` entry must also be listed in `covariates`: ",
      paste(missing_interest, collapse = ", "), "."
    )
  }

  validate_endpoint <- function(value, label) {
    if (length(value) != 1L || !is.numeric(value) ||
        is.na(value) || !is.finite(value)) {
      .pd_stop("`", label, "` must be one finite numeric time point.")
    }
    as.numeric(value)
  }
  baseline_time <- validate_endpoint(baseline_time, "baseline_time")
  cutoff_time <- validate_endpoint(cutoff_time, "cutoff_time")
  if (baseline_time > cutoff_time) {
    .pd_stop("`baseline_time` must not be after `cutoff_time`.")
  }

  if (length(y_type) != 1L || is.na(y_type)) {
    .pd_stop("`y_type` must be `\"C\"` (continuous) or `\"B\"` (binary).")
  }
  y_type <- toupper(as.character(y_type))
  if (!y_type %in% c("C", "B")) {
    .pd_stop("`y_type` must be `\"C\"` (continuous) or `\"B\"` (binary).")
  }

  structure(
    c(columns, list(
      baseline_time = baseline_time,
      cutoff_time = cutoff_time,
      covariates = covariates,
      interest_vars = interest_vars,
      y_type = y_type
    )),
    class = "pd_mapping"
  )
}

#' @rdname pd_methods
#' @export
print.pd_mapping <- function(x, ...) {
  cat("PDRobust data mapping and analysis settings.\n")
  labels <- c(
    id_col = "ID", time_col = "Time", A_col = "Treatment",
    S_col = "Survival", Y_col = "Outcome"
  )
  for (nm in names(labels)) {
    cat("  ", labels[[nm]], ": ", x[[nm]], "\n", sep = "")
  }
  cat("  Baseline time: ", .pd_time_label(x$baseline_time), "\n", sep = "")
  cat("  Cutoff time: ", .pd_time_label(x$cutoff_time), "\n", sep = "")
  cat(
    "  Mapped covariates: ",
    paste(x$covariates, collapse = ", "),
    "\n", sep = ""
  )
  cat(
    "  Interest variables: ",
    paste(x$interest_vars, collapse = ", "),
    "\n", sep = ""
  )
  cat(
    "  Outcome type: ", x$y_type,
    if (identical(x$y_type, "B")) " (binary)\n" else " (continuous)\n",
    sep = ""
  )
  invisible(x)
}

#' @noRd
.pd_validate_mapping <- function(mapping) {
  if (!inherits(mapping, "pd_mapping")) {
    .pd_stop("`mapping` must be returned by `Mapping()`.")
  }
  required <- c(
    "id_col", "time_col", "A_col", "S_col", "Y_col",
    "baseline_time", "cutoff_time",
    "covariates", "interest_vars", "y_type"
  )
  missing_fields <- setdiff(required, names(mapping))
  if (length(missing_fields)) {
    .pd_stop(
      "`mapping` is incomplete; missing: ",
      paste(missing_fields, collapse = ", "), "."
    )
  }
  if ("target_time" %in% names(mapping)) {
    .pd_stop(
      "`mapping` must not contain `target_time`; supply it only to `HTESepT()`."
    )
  }
  mapping
}

#' @noRd
.pd_mapping_from_data <- function(data) {
  value <- attr(data, "pd_mapping", exact = TRUE)
  if (inherits(value, "pd_mapping")) value else NULL
}

#' @noRd
.pd_mapping_for_data <- function(data, caller) {
  mapping <- .pd_mapping_from_data(data)
  if (is.null(mapping)) {
    .pd_stop(
      "`data` supplied to `", caller,
      "()` must be returned by `DataStandard()` and contain its mapping."
    )
  }
  .pd_validate_mapping(mapping)
}

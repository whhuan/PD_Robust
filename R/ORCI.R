#' Estimate treatment-group-specific survival odds ratios at cutoff
#'
#' Fits the supplied logistic model among subjects in the selected treatment
#' group at the mapped cutoff time.
#'
#' @param data A standardized `pd_data` object.
#' @param fomula Logistic-regression formula.
#' @param a Required cutoff treatment group, exactly `0` or `1`.
#' @param conf_level Confidence level.
#' @return An `odds_ratios` object containing three-decimal odds-ratio
#'   summaries, a full-precision fitted model, model diagnostics, and a plot.
#' @examples
#' \donttest{
#' data("BiSample", package = "PDRobust")
#' map <- Mapping(
#'   id = "id", time = "time", treatment = "A",
#'   survival = "S", outcome = "Y",
#'   baseline_time = 0, cutoff_time = 2,
#'   covariates = c("X1", "X2", "X4"),
#'   interest_vars = c("X1", "X2"), y_type = "B"
#' )
#' pd_dat <- DataStandard(BiSample, map)
#' result <- ORCI(
#'   pd_dat, S ~ X1 + X2 + X4, a = 0
#' )
#' result$forestplotdat
#' }
#' @export
ORCI <- function(data, fomula, a, conf_level = 0.95) {
  mapping <- .pd_require_prepared_data(data, "ORCI")
  if (length(a) != 1L || is.na(a) || !a %in% c(0, 1)) {
    .pd_stop("`a` must be exactly 0 or 1.")
  }
  if (length(conf_level) != 1L || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    .pd_stop("`conf_level` must be strictly between 0 and 1.")
  }

  data <- .pd_as_data_frame(data)
  fomula <- .pd_validate_formula(fomula, data, "fomula")
  fit_data <- data[
    data[[mapping$time_col]] == mapping$cutoff_time &
      data[[mapping$A_col]] == a,
    , drop = FALSE
  ]
  if (!nrow(fit_data)) {
    .pd_stop(
      "No cutoff observations were found for `a = ",
      a, "`."
    )
  }
  response <- .pd_formula_variables(fomula)[1L]
  if (!identical(response, mapping$S_col)) {
    .pd_stop(
      "`fomula` must use the mapped survival column `",
      mapping$S_col, "` as its response."
    )
  }
  fit <- .pd_fit_glm_checked(
    fomula, fit_data,
    label = "ORCI cutoff logistic model",
    allow_aliased = TRUE,
    diagnostic_context = list(
      analysis = "ORCI",
      sample = "original",
      treatment = a,
      n_subjects = length(unique(.pd_key(fit_data[[mapping$id_col]])))
    )
  )
  coefficient_all <- stats::coef(fit)
  standard_error_all <- sqrt(diag(stats::vcov(fit)))
  candidate <- names(coefficient_all) != "(Intercept)"
  keep <- candidate & is.finite(coefficient_all) &
    is.finite(standard_error_all)
  if (any(candidate & !keep)) {
    .pd_warn("Aliased or non-finite coefficients were omitted.")
  }
  coefficient <- coefficient_all[keep]
  standard_error <- standard_error_all[keep]
  if (!length(coefficient)) {
    .pd_stop("No estimable non-intercept coefficient is available.")
  }

  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  result_data <- data.frame(
    covname = names(coefficient),
    estcoef = exp(coefficient),
    lowerbd = exp(coefficient - z * standard_error),
    upperbd = exp(coefficient + z * standard_error),
    stringsAsFactors = FALSE
  )
  finite_interval <- is.finite(result_data$estcoef) &
    is.finite(result_data$lowerbd) &
    is.finite(result_data$upperbd)
  if (any(!finite_interval)) {
    .pd_warn(
      "Aliased or otherwise non-finite odds-ratio intervals were omitted."
    )
    result_data <- result_data[finite_interval, , drop = FALSE]
  }
  if (!nrow(result_data)) {
    .pd_stop("No finite non-intercept odds ratio is available.")
  }
  result_data <- .pd_round_output_columns(
    result_data, c("estcoef", "lowerbd", "upperbd")
  )
  result_data$covname <- factor(
    result_data$covname, levels = rev(result_data$covname)
  )
  colours <- .pd_named_colours(result_data$covname)
  plot <- ggplot2::ggplot(
    result_data,
    ggplot2::aes(x = estcoef, y = covname, colour = covname)
  ) +
    ggplot2::geom_vline(
      xintercept = 1, linetype = "dashed", colour = "grey60"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = lowerbd, xmax = upperbd),
      orientation = "y", width = 0.18
    ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(
      x = paste0("Odds ratio (", round(100 * conf_level, 3), "% CI)"),
      y = NULL, colour = "Covariate"
    ) +
    ggplot2::scale_colour_manual(values = colours, drop = FALSE) +
    ggplot2::theme_minimal(base_size = 12)

  result <- list(
    forestplotdat = result_data,
    model = fit,
    model_diagnostics = attr(fit, "pd_model_diagnostics", exact = TRUE),
    warnings = attr(fit, "pd_warnings", exact = TRUE),
    analysis_data = fit_data,
    plot = plot,
    mapping = mapping,
    settings = list(
      a = as.numeric(a),
      conf_level = conf_level
    ),
    call = match.call()
  )
  class(result) <- c("pd_odds_ratios", "odds_ratios")
  result
}

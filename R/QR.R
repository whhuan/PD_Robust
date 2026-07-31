#' Summarize cutoff covariates in the always-survivor principal stratum
#'
#' Restores the original weighted intercept-only quantile-regression algorithm.
#' For each nonbinary mapped interest variable, `quantreg::rq(variable ~ 1,
#' weights = K_p0, tau = quantile_level)` is fitted on cutoff rows.
#'
#' @param data A standardized `pd_data` object.
#' @param prin_fo Principal-score formula.
#' @param quantile_level Quantile probabilities strictly between zero and one.
#' @return A `QR` object containing three-decimal weighted means and quantiles;
#'   principal-score weights retain full precision.
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
#' result <- QR(
#'   pd_dat,
#'   S ~ X1 + X2 + X4 + A + time,
#'   quantile_level = c(0.25, 0.5, 0.75)
#' )
#' result$mean
#' }
#' @export
QR <- function(data, prin_fo, quantile_level = 0.5) {
  mapping <- .pd_require_prepared_data(data, "QR")
  interest_vars <- .pd_validate_interest_vars(data, "QR", mapping)
  if (!is.numeric(quantile_level) || !length(quantile_level) ||
      any(!is.finite(quantile_level)) ||
      any(quantile_level <= 0 | quantile_level >= 1)) {
    .pd_stop(
      "`quantile_level` must contain finite probabilities strictly between 0 and 1."
    )
  }

  panel <- .pd_as_data_frame(data)
  panel <- panel[
    panel[[mapping$time_col]] %in% .pd_analysis_times(panel, mapping),
    , drop = FALSE
  ]
  panel <- panel[order(
    panel[[mapping$id_col]], panel[[mapping$time_col]]
  ), , drop = FALSE]
  prin_fo <- .pd_validate_formula(prin_fo, panel, "prin_fo")
  cutoff <- panel[
    panel[[mapping$time_col]] == mapping$cutoff_time,
    , drop = FALSE
  ]
  cutoff <- cutoff[order(cutoff[[mapping$id_col]]), , drop = FALSE]
  if (!nrow(cutoff) || anyDuplicated(.pd_key(cutoff[[mapping$id_col]]))) {
    .pd_stop("Exactly one cutoff row per subject is required.")
  }

  p0_all <- .pd_prinpred_impl(
    prin_fo, panel, panel, a = 0, mapping = mapping
  )
  cutoff_index <- which(panel[[mapping$time_col]] == mapping$cutoff_time)
  raw_cutoff <- panel[[mapping$id_col]][cutoff_index]
  K_p0 <- as.numeric(p0_all[cutoff_index][
    match(.pd_key(cutoff[[mapping$id_col]]), .pd_key(raw_cutoff))
  ])
  if (any(!is.finite(K_p0)) || mean(K_p0) <= 0) {
    .pd_stop("Cutoff principal-score weights are invalid.")
  }

  X <- as.matrix(cutoff[interest_vars])
  weighted_means <- colMeans((K_p0 / mean(K_p0)) * X)
  names(weighted_means) <- interest_vars

  quantiles <- stats::setNames(vector("list", length(interest_vars)), interest_vars)
  binary <- stats::setNames(logical(length(interest_vars)), interest_vars)
  for (variable in interest_vars) {
    unique_values <- unique(stats::na.omit(cutoff[[variable]]))
    binary[[variable]] <- length(unique_values) <= 2L
    if (binary[[variable]]) {
      quantiles[[variable]] <- stats::setNames(
        rep(NA_real_, length(quantile_level)),
        paste0("q", .pd_time_label(quantile_level))
      )
    } else {
      fit_formula <- stats::as.formula(sprintf("%s ~ 1", variable))
      fit <- .pd_fit_rq_checked(
        fit_formula, cutoff, weights = K_p0,
        tau = quantile_level,
        label = paste("QR covariate", variable)
      )
      value <- as.numeric(stats::coef(fit))
      quantiles[[variable]] <- stats::setNames(
        value, paste0("q", .pd_time_label(quantile_level))
      )
    }
  }

  tidy <- do.call(rbind, lapply(interest_vars, function(variable) {
    data.frame(
      covariate = variable,
      mean = unname(weighted_means[[variable]]),
      quantile = quantile_level,
      estimate = as.numeric(quantiles[[variable]]),
      binary = binary[[variable]],
      stringsAsFactors = FALSE
    )
  }))
  weighted_means_output <- .pd_round_output(weighted_means)
  quantiles_output <- lapply(quantiles, .pd_round_output)
  names(quantiles_output) <- names(quantiles)
  tidy <- .pd_round_output_columns(tidy, c("mean", "estimate"))

  result <- list(
    mean = weighted_means_output,
    quantile = quantiles_output,
    binary = binary,
    data = tidy,
    weights = K_p0,
    formula = prin_fo,
    mapping = mapping,
    call = match.call()
  )
  class(result) <- c("pd_principal_summary", "QR")
  result
}

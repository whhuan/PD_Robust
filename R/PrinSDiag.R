#' Diagnose principal-score balance
#'
#' Refits the propensity and principal-score models internally. Estimated
#' propensity scores are always clipped to `[0.01, 0.99]`. Cumulative principal
#' scores use all actual observed times from baseline through cutoff, and the
#' diagnostic equation is evaluated at cutoff using the original algorithm.
#'
#' @param data A standardized `pd_data` object.
#' @param ps_fo Propensity-score formula; its numeric covariates are diagnosed.
#' @param prin_fo Principal-score formula.
#' @return A `PrinSDiag` object containing three-decimal standardized
#'   statistics and plots; nuisance probabilities retain full precision.
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
#' result <- PrinSDiag(
#'   pd_dat,
#'   A ~ X1 + X2 + X4,
#'   S ~ X1 + X2 + X4 + A + time
#' )
#' result$statistics
#' }
#' @export
PrinSDiag <- function(data, ps_fo, prin_fo) {
  mapping <- .pd_require_prepared_data(data, "PrinSDiag")
  panel <- .pd_as_data_frame(data)
  panel <- panel[
    panel[[mapping$time_col]] %in% .pd_analysis_times(panel, mapping),
    , drop = FALSE
  ]
  panel <- panel[order(
    panel[[mapping$id_col]], panel[[mapping$time_col]]
  ), , drop = FALSE]
  ps_fo <- .pd_validate_formula(ps_fo, panel, "ps_fo")
  prin_fo <- .pd_validate_formula(prin_fo, panel, "prin_fo")
  covariates <- .pd_formula_covariates(ps_fo, mapping, "propensity-score")
  nonnumeric <- covariates[
    !vapply(panel[covariates], is.numeric, logical(1))
  ]
  if (length(nonnumeric)) {
    .pd_stop(
      "Principal diagnostics require numeric covariates: ",
      paste(nonnumeric, collapse = ", "), "."
    )
  }

  components <- .pd_hte_components(panel, ps_fo, prin_fo, mapping)
  pi <- components$pi
  if (any(!is.finite(pi))) {
    .pd_stop("Propensity-score predictions must be finite.")
  }
  pi <- pmin(pmax(pi, 0.01), 0.99)
  p0 <- components$p0
  p1 <- components$p1
  if (any(pi <= 0 | pi >= 1) || any(p1 <= 0)) {
    .pd_stop("Raw probabilities create a zero diagnostic denominator.")
  }
  A <- as.numeric(components$cutoff[[mapping$A_col]])
  S <- as.numeric(components$cutoff[[mapping$S_col]])
  e11 <- mean(p0)
  if (!is.finite(e11) || e11 <= 0) {
    .pd_stop("Estimated always-survivor prevalence is nonpositive.")
  }

  X <- as.matrix(components$cutoff[covariates])
  w_left <- p0 * S * A / (e11 * p1 * pi)
  w_right <- S * (1 - A) / (e11 * (1 - pi))
  D <- X * w_left - X * w_right
  denominator <- apply(D, 2L, stats::sd, na.rm = TRUE)
  statistic <- sqrt(nrow(D)) * colMeans(D, na.rm = TRUE) / denominator
  statistic[!is.finite(statistic)] <- NA_real_
  statistic_output <- .pd_round_output(statistic)

  plot_data <- data.frame(
    covariate = covariates,
    statistic = as.numeric(statistic_output),
    stringsAsFactors = FALSE
  )
  # Retain all diagnostic statistics in the returned object, including NA
  # values that identify non-estimable covariates, but exclude non-finite
  # statistics from the point layer so normal plotting does not emit a
  # ggplot2 missing-value warning.
  finite_plot_data <- plot_data[is.finite(plot_data$statistic), , drop = FALSE]
  plot <- ggplot2::ggplot(
    finite_plot_data,
    ggplot2::aes(x = covariate, y = statistic, colour = statistic)
  ) +
    ggplot2::geom_hline(
      yintercept = c(-1.96, 1.96),
      linetype = "dashed", colour = "grey55"
    ) +
    ggplot2::geom_point(size = 2.7) +
    ggplot2::labs(
      x = "Covariate", y = "Standardized statistic", colour = "Statistic"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  result <- list(
    pripfigdat = plot_data,
    statistics = stats::setNames(as.numeric(statistic_output), covariates),
    propensity = pi,
    p0 = p0,
    p1 = p1,
    plot = plot,
    formulas = list(propensity = ps_fo, principal = prin_fo),
    mapping = mapping,
    call = match.call()
  )
  class(result) <- c("pd_principal_diagnostic", "PrinSDiag")
  result
}

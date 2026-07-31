#' Diagnose propensity-score covariate balance
#'
#' Fits the propensity-score model internally on baseline observations, clips
#' every estimated propensity score to `[0.01, 0.99]`, creates ordinary
#' inverse-probability-of-treatment weights, and evaluates balance using the
#' original pooled and weighted-ESS SMD denominators.
#'
#' @param data A standardized `pd_data` object.
#' @param ps_fo Propensity-score formula.
#' @return A `PSDiag` object containing three-decimal SMD summaries and a plot;
#'   propensity scores and weights retain full precision.
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
#' result <- PSDiag(pd_dat, A ~ X1 + X2 + X4)
#' result$smd_after
#' }
#' @export
PSDiag <- function(data, ps_fo) {
  mapping <- .pd_require_prepared_data(data, "PSDiag")
  data <- .pd_as_data_frame(data)
  ps_fo <- .pd_validate_formula(ps_fo, data, "ps_fo")
  covariates <- .pd_formula_covariates(ps_fo, mapping, "propensity-score")
  .pd_assert_columns(data, c(
    mapping$id_col, mapping$time_col, mapping$A_col, covariates
  ))
  baseline <- data[
    data[[mapping$time_col]] == mapping$baseline_time,
    , drop = FALSE
  ]
  baseline <- baseline[order(baseline[[mapping$id_col]]), , drop = FALSE]
  if (!nrow(baseline) || anyDuplicated(.pd_key(baseline[[mapping$id_col]]))) {
    .pd_stop("Exactly one baseline observation per subject is required.")
  }
  nonnumeric <- covariates[
    !vapply(baseline[covariates], is.numeric, logical(1))
  ]
  if (length(nonnumeric)) {
    .pd_stop(
      "Propensity-score diagnostics require numeric covariates: ",
      paste(nonnumeric, collapse = ", "), "."
    )
  }

  pi <- as.numeric(.pd_pspred_impl(ps_fo, data, baseline, mapping))
  if (any(!is.finite(pi))) {
    .pd_stop("Propensity-score predictions must be finite.")
  }
  pi <- pmin(pmax(pi, 0.01), 0.99)
  A <- as.numeric(baseline[[mapping$A_col]])
  if (!all(A %in% c(0, 1)) || length(unique(A)) < 2L) {
    .pd_stop("Both treatment groups are required for `PSDiag()`.")
  }
  weight <- A / pi + (1 - A) / (1 - pi)
  X <- baseline[covariates]
  smd_before <- .pd_smd(X, A)
  smd_after <- .pd_smd(X, A, weight)
  smd_before_output <- .pd_round_output(smd_before)
  smd_after_output <- .pd_round_output(smd_after)

  plot_data <- data.frame(
    covariate = rep(covariates, 2L),
    adjustment = rep(c("Before", "After"), each = length(covariates)),
    smd = abs(c(smd_before_output, smd_after_output)),
    stringsAsFactors = FALSE
  )
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = covariate, y = smd, colour = adjustment)
  ) +
    ggplot2::geom_hline(
      yintercept = 0.1, linetype = "dashed", colour = "grey60"
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.4), size = 2.5
    ) +
    ggplot2::labs(
      x = "Covariate", y = "Absolute standardized mean difference",
      colour = "Adjustment"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  result <- list(
    smd_before = smd_before_output,
    smd_after = smd_after_output,
    weights = weight,
    weight_type = "ordinary IPTW",
    propensity = pi,
    data = plot_data,
    plot = plot,
    formula = ps_fo,
    mapping = mapping,
    call = match.call()
  )
  class(result) <- c("pd_exposure_diagnostic", "PSDiag")
  result
}

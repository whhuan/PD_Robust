#' Perform outcome-noise sensitivity analysis
#'
#' Restores the original sensitivity-analysis equations and variance definition.
#' At each actual observed time from baseline through cutoff, the perturbation
#' variance is the ordinary variance of all observed outcomes at that time.
#' Both cutoff treatment groups enter the estimating equations.
#'
#' Continuous outcomes retain the original additive-noise implementation:
#' the perturbed outcomes are used both to refit the linear outcome model and
#' in the estimating equation. For binary outcomes, additive perturbations are
#' applied to the estimating-equation outcome while the logistic nuisance model
#' is fitted to the original 0/1 outcomes. This keeps the outcome model
#' binomial rather than fitting a logistic model to invalid pseudo-responses.
#' Binary HTE coefficients use the same bounded-link estimating equation as
#' `HTESepT()`.
#'
#' All three prediction models are refitted internally; no fitted model is
#' cached or reused across calls. Within one scenario, a model fitted to the
#' same rows and formula is reused only to obtain the two counterfactual
#' treatment predictions.
#'
#' @param data A standardized continuous- or binary-outcome `pd_data` object.
#' @param ps_fo Propensity-score formula.
#' @param prin_fo Principal-score formula.
#' @param out_fo Outcome-model formula.
#' @param ratiovec Finite nonnegative outcome-variance ratios.
#' @return An `SA` object containing rounded tidy and wide estimates,
#'   full-precision estimating diagnostics, consolidated warnings, and plots.
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
#' result <- SA(
#'   pd_dat,
#'   A ~ X1 + X2 + X4,
#'   S ~ X1 + X2 + X4 + A + time,
#'   Y ~ X1 + X2 + A,
#'   ratiovec = c(0, 0.05)
#' )
#' head(result$data)
#' }
#' @export
SA <- function(data, ps_fo, prin_fo, out_fo,
               ratiovec = c(0, 0.05, 0.10)) {
  captured <- .pd_capture_conditions(
    .pd_sa_impl(data, ps_fo, prin_fo, out_fo, ratiovec)
  )
  if (inherits(captured$value, "error")) {
    .pd_stop(conditionMessage(captured$value))
  }
  .pd_emit_analysis_warnings(captured$warnings, "SA()")
  result <- captured$value
  result$warnings <- captured$warnings
  result$model_diagnostics <- .pd_bind_model_diagnostics(
    result$model_diagnostics,
    analysis = "SA",
    sample = "original"
  )
  result$data <- .pd_round_output_columns(result$data, "estimate")
  coefficient_columns <- setdiff(
    names(result$beta_df_wide), c("ratiovec", "time")
  )
  result$beta_df_wide <- .pd_round_output_columns(
    result$beta_df_wide, coefficient_columns
  )
  result$variance_by_time <- .pd_round_output(result$variance_by_time)
  result$plot <- .pd_sa_plots(result$data, result$mapping)
  result
}

#' Full-precision sensitivity-analysis implementation
#'
#' @noRd
.pd_sa_impl <- function(data, ps_fo, prin_fo, out_fo, ratiovec) {
  mapping <- .pd_require_prepared_data(data, "SA")
  if (!is.numeric(ratiovec) || !length(ratiovec) ||
      any(!is.finite(ratiovec)) || any(ratiovec < 0)) {
    .pd_stop("`ratiovec` must contain finite nonnegative values.")
  }

  prep <- .pd_prepare_hte(data, caller = "SA")
  panel <- prep$panel
  mapping <- prep$mapping
  ps_fo <- .pd_validate_formula(ps_fo, panel, "ps_fo")
  prin_fo <- .pd_validate_formula(prin_fo, panel, "prin_fo")
  out_fo <- .pd_validate_formula(out_fo, panel, "out_fo")
  .pd_validate_model_covariates(ps_fo, prin_fo, out_fo, mapping)
  components <- .pd_hte_components(panel, ps_fo, prin_fo, mapping)
  weights <- .pd_hte_weights(components, mapping)
  model_diagnostics <- components$model_diagnostics
  model_diagnostics$analysis <- "SA"
  coefficient_names <- c("Intercept", mapping$interest_vars)
  N <- length(components$ids)

  variance_by_time <- stats::setNames(
    vapply(prep$all_times, function(time_value) {
      stats::var(
        panel[[mapping$Y_col]][panel[[mapping$time_col]] == time_value],
        na.rm = TRUE
      )
    }, numeric(1)),
    .pd_time_label(prep$all_times)
  )
  invalid_variance <- names(variance_by_time)[
    !is.finite(variance_by_time) | variance_by_time < 0
  ]
  if (length(invalid_variance)) {
    .pd_stop(
      "Outcome variance is not estimable at analysis times: ",
      paste(invalid_variance, collapse = ", "), "."
    )
  }

  rows <- list()
  row_index <- 0L
  convergence <- list()
  for (ratio in ratiovec) {
    for (time_value in prep$all_times) {
      scenario <- paste(
        .pd_time_label(ratio),
        .pd_time_label(time_value),
        sep = "@"
      )
      current <- .pd_align_time_rows(
        panel, time_value, components$ids, mapping, "Sensitivity-analysis"
      )
      unperturbed <- current
      noise <- stats::rnorm(
        N, mean = 0,
        sd = sqrt(variance_by_time[[.pd_time_label(time_value)]] * ratio)
      )
      current[[mapping$Y_col]] <- current[[mapping$Y_col]] - noise
      alive_ids <- components$cutoff[[mapping$id_col]][
        components$cutoff[[mapping$S_col]] == 1
      ]
      fit_source <- if (identical(mapping$y_type, "B")) {
        unperturbed
      } else {
        current
      }
      fit_dat <- fit_source[
        .pd_key(fit_source[[mapping$id_col]]) %in% .pd_key(alive_ids),
        , drop = FALSE
      ]
      phi_diff <- .pd_hte_phi(current, fit_dat, weights, out_fo, mapping)
      scenario_diagnostics <- .pd_model_diagnostics(phi_diff)
      scenario_diagnostics$analysis <- "SA"
      model_diagnostics <- .pd_bind_model_diagnostics(
        model_diagnostics, scenario_diagnostics
      )
      if (identical(mapping$y_type, "B")) {
        solved <- .pd_solve_binary_score(
          X = components$X,
          phi_diff = phi_diff,
          psi_s0 = weights$psi_s0,
          start = rep(0, ncol(components$X)),
          max_iter = 100L,
          tolerance = 1e-6
        )
        if (!isTRUE(solved$converged)) {
          .pd_warn(
            "Binary SA estimating equation did not fully converge at ratio ",
            .pd_time_label(ratio), " and time ",
            .pd_time_label(time_value), "."
          )
        }
        beta <- solved$root
        convergence[[scenario]] <- solved
      } else {
        beta <- .pd_solve_checked(
          crossprod(components$X, components$X * weights$psi_s0),
          crossprod(components$X, phi_diff),
          paste0(
            "SA at ratio ", .pd_time_label(ratio),
            " and time ", .pd_time_label(time_value)
          )
        )
        convergence[[scenario]] <- list(
          converged = TRUE,
          iterations = 1L,
          solver = "closed-form weighted estimating equation"
        )
      }
      names(beta) <- coefficient_names

      for (term in coefficient_names) {
        row_index <- row_index + 1L
        rows[[row_index]] <- data.frame(
          ratio = ratio,
          time = time_value,
          term = term,
          estimate = beta[[term]],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  tidy <- do.call(rbind, rows)
  group_key <- interaction(tidy$ratio, tidy$time, drop = TRUE)
  wide <- do.call(rbind, lapply(split(tidy, group_key), function(group) {
    values <- stats::setNames(group$estimate, group$term)
    data.frame(
      ratiovec = group$ratio[1L],
      time = group$time[1L],
      as.list(values),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
  rownames(wide) <- NULL

  plots <- .pd_sa_plots(tidy, mapping)

  result <- list(
    beta_df_wide = wide,
    data = tidy,
    plot = plots,
    variance_by_time = variance_by_time,
    convergence = convergence,
    model_diagnostics = model_diagnostics,
    formulas = list(
      propensity = ps_fo, principal = prin_fo, outcome = out_fo
    ),
    mapping = mapping,
    settings = list(
      ratiovec = ratiovec,
      outcome_type = mapping$y_type
    ),
    call = match.call()
  )
  class(result) <- c("pd_sensitivity", "SA")
  result
}

#' @noRd
.pd_sa_plots <- function(tidy, mapping) {
  plots <- list()
  for (term in mapping$interest_vars) {
    plot_data <- tidy[tidy$term == term, , drop = FALSE]
    plot_data$ratio <- factor(plot_data$ratio)
    plots[[term]] <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = time, y = estimate, colour = ratio,
        linetype = ratio, group = ratio
      )
    ) +
      ggplot2::geom_hline(
        yintercept = 0, linetype = "dashed", colour = "grey70"
      ) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2.2) +
      ggplot2::labs(
        x = "Time", y = "HTE estimate",
        colour = "Variance ratio", linetype = "Variance ratio",
        title = paste("Sensitivity curve for", term)
      ) +
      ggplot2::theme_minimal(base_size = 12)
  }
  plots
}

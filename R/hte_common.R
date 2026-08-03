# Internal HTE implementation -------------------------------------------------

#' @noRd
.pd_prepare_hte <- function(data, caller = "HTE") {
  mapping <- .pd_mapping_for_data(data, caller)
  data <- .pd_as_data_frame(data)
  .pd_assert_nonempty(data)
  .pd_validate_interest_vars(data, caller, mapping)

  required <- c(
    mapping$id_col, mapping$time_col, mapping$A_col,
    mapping$S_col, mapping$Y_col, mapping$interest_vars
  )
  .pd_assert_columns(data, required)

  all_times <- .pd_analysis_times(data, mapping)
  panel <- data[
    data[[mapping$time_col]] %in% all_times,
    , drop = FALSE
  ]
  panel <- panel[order(
    panel[[mapping$id_col]], panel[[mapping$time_col]], na.last = TRUE
  ), , drop = FALSE]
  rownames(panel) <- NULL
  attr(panel, "pd_mapping") <- mapping
  class(panel) <- unique(c("pd_data", class(panel)))

  list(
    panel = panel,
    mapping = mapping,
    all_times = all_times
  )
}

#' @noRd
.pd_align_time_rows <- function(panel, time_value, ids, mapping, label) {
  rows <- panel[
    panel[[mapping$time_col]] == time_value,
    , drop = FALSE
  ]
  rows <- rows[
    match(.pd_key(ids), .pd_key(rows[[mapping$id_col]])),
    , drop = FALSE
  ]
  if (nrow(rows) != length(ids) || anyNA(rows[[mapping$id_col]])) {
    .pd_stop(
      label, " rows could not be aligned by subject ID at time ",
      .pd_time_label(time_value), "."
    )
  }
  rows
}

#' @noRd
.pd_hte_components <- function(panel, ps_fo, prin_fo, mapping) {
  id_col <- mapping$id_col
  time_col <- mapping$time_col

  baseline <- panel[
    panel[[time_col]] == mapping$baseline_time,
    , drop = FALSE
  ]
  baseline <- baseline[order(baseline[[id_col]]), , drop = FALSE]
  if (anyDuplicated(.pd_key(baseline[[id_col]]))) {
    .pd_stop("Baseline contains duplicate subject records.")
  }
  ids <- baseline[[id_col]]
  cutoff <- .pd_align_time_rows(
    panel, mapping$cutoff_time, ids, mapping, "Cutoff"
  )

  pi_prediction <- .pd_pspred_impl(
    ps_fo = ps_fo,
    fit_dat = panel,
    pred_dat = baseline,
    mapping = mapping
  )
  principal_predictions <- .pd_prinpred_pair_impl(
    prin_fo = prin_fo,
    fit_dat = panel,
    pred_dat = panel,
    mapping = mapping
  )
  p0_all <- principal_predictions$p0
  p1_all <- principal_predictions$p1

  cutoff_index <- which(panel[[time_col]] == mapping$cutoff_time)
  cutoff_ids <- panel[[id_col]][cutoff_index]
  index <- match(.pd_key(ids), .pd_key(cutoff_ids))
  if (anyNA(index)) {
    .pd_stop("Cumulative principal scores could not be aligned at cutoff.")
  }
  p0 <- as.numeric(p0_all[cutoff_index][index])
  p1 <- as.numeric(p1_all[cutoff_index][index])
  pi <- as.numeric(pi_prediction)
  if (any(!is.finite(c(pi, p0, p1)))) {
    .pd_stop("Prediction values are missing or non-finite after ID alignment.")
  }

  X <- cbind(
    Intercept = 1,
    as.matrix(baseline[mapping$interest_vars])
  )
  storage.mode(X) <- "double"

  list(
    baseline = baseline,
    cutoff = cutoff,
    ids = ids,
    keys = .pd_key(ids),
    pi = as.numeric(pi),
    p0 = p0,
    p1 = p1,
    X = X,
    model_diagnostics = .pd_bind_model_diagnostics(
      .pd_model_diagnostics(pi_prediction),
      principal_predictions$model_diagnostics
    )
  )
}

#' @noRd
.pd_hte_weights <- function(components, mapping) {
  A <- as.numeric(components$cutoff[[mapping$A_col]])
  S_cutoff <- as.numeric(components$cutoff[[mapping$S_col]])
  pi <- pmin(pmax(components$pi, 0.01), 0.99)
  p0 <- components$p0
  p1 <- components$p1

  psi_s0 <- (A == 0) * (S_cutoff - p0) / (1 - pi) + p0
  pip1 <- pmin(pmax(p1 * pi, 0.005), 0.995)

  list(
    A = A,
    S_cutoff = S_cutoff,
    pi = pi,
    p0 = p0,
    p1 = p1,
    psi_s0 = psi_s0,
    pip1 = pip1
  )
}

#' @noRd
.pd_outcome_fit_rows <- function(panel, components, mapping) {
  alive_ids <- components$cutoff[[mapping$id_col]][
    components$cutoff[[mapping$S_col]] == 1
  ]
  panel[.pd_key(panel[[mapping$id_col]]) %in% .pd_key(alive_ids), , drop = FALSE]
}

#' @noRd
.pd_hte_phi <- function(current, fit_dat, weights, out_fo, mapping) {
  outcome_predictions <- .pd_outpred_pair_impl(
    out_fo = out_fo,
    fit_dat = fit_dat,
    pred_dat = current,
    mapping = mapping
  )
  mu0_prediction <- outcome_predictions$mu0
  mu1_prediction <- outcome_predictions$mu1
  mu0 <- as.numeric(mu0_prediction)
  mu1 <- as.numeric(mu1_prediction)

  y <- .pd_structural_outcome(
    current[[mapping$Y_col]],
    weights$S_cutoff == 1,
    context = paste0("time ", .pd_time_label(current[[mapping$time_col]][1L]))
  )
  phi1 <- weights$p0 * weights$S_cutoff * weights$A *
    (y - mu1) / weights$pip1 + mu1 * weights$psi_s0
  phi0 <- (weights$A == 0) *
    (y * weights$S_cutoff - mu0 * weights$p0) /
    (1 - weights$pi) + mu0 * weights$p0

  out <- as.numeric(phi1 - phi0)
  diagnostics <- outcome_predictions$model_diagnostics
  diagnostics$target_time <- as.numeric(current[[mapping$time_col]][1L])
  attr(out, "pd_model_diagnostics") <- diagnostics
  out
}

#' @noRd
.pd_htesep_once <- function(data, ps_fo, prin_fo, out_fo, target_time) {
  prep <- .pd_prepare_hte(data, caller = "HTESepT")
  panel <- prep$panel
  mapping <- prep$mapping
  target_time <- .pd_validate_target_time(target_time, panel, mapping)
  ps_fo <- .pd_validate_formula(ps_fo, panel, "ps_fo")
  prin_fo <- .pd_validate_formula(prin_fo, panel, "prin_fo")
  out_fo <- .pd_validate_formula(out_fo, panel, "out_fo")
  .pd_validate_model_covariates(ps_fo, prin_fo, out_fo, mapping)

  components <- .pd_hte_components(panel, ps_fo, prin_fo, mapping)
  weights <- .pd_hte_weights(components, mapping)
  model_diagnostics <- components$model_diagnostics
  model_diagnostics$analysis <- "HTESepT"
  coefficient_names <- c("Intercept", mapping$interest_vars)
  estimate <- matrix(
    NA_real_,
    nrow = length(target_time),
    ncol = length(coefficient_names),
    dimnames = list(.pd_time_label(target_time), coefficient_names)
  )
  convergence <- vector("list", length(target_time))

  for (i in seq_along(target_time)) {
    time_value <- target_time[i]
    current <- .pd_align_time_rows(
      panel, time_value, components$ids, mapping, "Target-time"
    )
    alive_ids <- components$cutoff[[mapping$id_col]][
      components$cutoff[[mapping$S_col]] == 1
    ]
    fit_dat <- current[
      .pd_key(current[[mapping$id_col]]) %in% .pd_key(alive_ids),
      , drop = FALSE
    ]
    phi_diff <- .pd_hte_phi(current, fit_dat, weights, out_fo, mapping)
    model_diagnostics <- .pd_bind_model_diagnostics(
      model_diagnostics,
      .pd_model_diagnostics(phi_diff)
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
      estimate[i, ] <- solved$root
      convergence[[i]] <- solved
    } else {
      estimate[i, ] <- .pd_solve_checked(
        crossprod(components$X, components$X * weights$psi_s0),
        crossprod(components$X, phi_diff),
        paste0("HTESepT at time ", .pd_time_label(time_value))
      )
      convergence[[i]] <- list(
        converged = TRUE,
        iterations = 1L,
        solver = "closed-form weighted estimating equation"
      )
    }
  }
  model_diagnostics$analysis <- "HTESepT"

  list(
    estimate = estimate,
    convergence = convergence,
    mapping = mapping,
    target_time = target_time,
    model_diagnostics = model_diagnostics
  )
}

#' @noRd
.pd_hteall_once <- function(data, ps_fo, prin_fo, out_fo) {
  prep <- .pd_prepare_hte(data, caller = "HTEAllT")
  panel <- prep$panel
  mapping <- prep$mapping
  ps_fo <- .pd_validate_formula(ps_fo, panel, "ps_fo")
  prin_fo <- .pd_validate_formula(prin_fo, panel, "prin_fo")
  out_fo <- .pd_validate_formula(out_fo, panel, "out_fo")
  .pd_validate_model_covariates(ps_fo, prin_fo, out_fo, mapping)

  components <- .pd_hte_components(panel, ps_fo, prin_fo, mapping)
  weights <- .pd_hte_weights(components, mapping)
  model_diagnostics <- components$model_diagnostics
  model_diagnostics$analysis <- "HTEAllT"
  subject_index <- match(
    .pd_key(panel[[mapping$id_col]]), components$keys
  )
  if (anyNA(subject_index)) {
    .pd_stop("Longitudinal rows could not be aligned to baseline subjects.")
  }

  X <- cbind(
    Intercept = 1,
    as.matrix(components$baseline[mapping$interest_vars])[
      subject_index, , drop = FALSE
    ]
  )
  time_effect_estimable <- length(prep$all_times) > 1L
  if (time_effect_estimable) {
    X <- cbind(X, `Time Effect` = as.numeric(panel[[mapping$time_col]]))
  }
  storage.mode(X) <- "double"

  long_weights <- lapply(weights, function(value) {
    if (length(value) == length(components$ids)) value[subject_index] else value
  })
  fit_dat <- .pd_outcome_fit_rows(panel, components, mapping)
  outcome_predictions <- .pd_outpred_pair_impl(
    out_fo, fit_dat, panel, mapping = mapping
  )
  mu0_prediction <- outcome_predictions$mu0
  mu1_prediction <- outcome_predictions$mu1
  model_diagnostics <- .pd_bind_model_diagnostics(
    model_diagnostics,
    outcome_predictions$model_diagnostics
  )
  mu0 <- as.numeric(mu0_prediction)
  mu1 <- as.numeric(mu1_prediction)
  model_diagnostics$analysis <- "HTEAllT"
  y <- .pd_structural_outcome(
    panel[[mapping$Y_col]],
    long_weights$S_cutoff == 1,
    context = "the pooled baseline-to-cutoff analysis"
  )

  phi1 <- long_weights$p0 * long_weights$S_cutoff * long_weights$A *
    (y - mu1) / long_weights$pip1 + mu1 * long_weights$psi_s0
  phi0 <- (long_weights$A == 0) *
    (y * long_weights$S_cutoff - mu0 * long_weights$p0) /
    (1 - long_weights$pi) + mu0 * long_weights$p0
  phi_diff <- as.numeric(phi1 - phi0)

  if (identical(mapping$y_type, "B")) {
    solved <- .pd_solve_binary_score(
      X = X,
      phi_diff = phi_diff,
      psi_s0 = long_weights$psi_s0,
      start = rep(0, ncol(X)),
      max_iter = 100L,
      tolerance = 1e-6
    )
    beta <- solved$root
    convergence <- solved
  } else {
    beta <- .pd_solve_checked(
      crossprod(X, X * long_weights$psi_s0),
      crossprod(X, phi_diff),
      "HTEAllT"
    )
    convergence <- list(
      converged = TRUE,
      iterations = 1L,
      solver = "closed-form weighted estimating equation"
    )
  }
  names(beta) <- colnames(X)

  list(
    estimate = beta,
    convergence = convergence,
    mapping = mapping,
    analysis_times = prep$all_times,
    time_effect_estimable = time_effect_estimable,
    model_diagnostics = model_diagnostics,
    note = if (time_effect_estimable) NULL else
      "Time effect cannot be estimated because only one analysis time point is available."
  )
}

# Bootstrap progress --------------------------------------------------------

#' Validate an optional bootstrap progress callback
#'
#' @param progress_callback `NULL` or a function that accepts one named list.
#' @return The callback, unchanged, or `NULL`.
#' @noRd
.pd_validate_progress_callback <- function(progress_callback) {
  if (!is.null(progress_callback) && !is.function(progress_callback)) {
    .pd_stop("`progress_callback` must be NULL or a function.")
  }
  progress_callback
}

#' Emit one nonintrusive bootstrap progress update
#'
#' Callback failures must not change the scientific analysis. The first
#' callback error therefore produces one warning and disables further updates
#' for that run. Returning the callback lets callers retain or disable it.
#'
#' @noRd
.pd_emit_bootstrap_progress <- function(
    progress_callback,
    stage,
    successful,
    requested,
    attempts,
    max_attempts,
    started_at) {
  if (is.null(progress_callback)) return(NULL)

  now <- Sys.time()
  update <- list(
    stage = as.character(stage),
    successful = as.integer(successful),
    requested = as.integer(requested),
    attempts = as.integer(attempts),
    max_attempts = as.integer(max_attempts),
    failed_attempts = as.integer(attempts - successful),
    complete = identical(as.character(stage), "completed") &&
      as.integer(successful) >= as.integer(requested),
    elapsed_seconds = as.numeric(difftime(now, started_at, units = "secs")),
    updated_at = now
  )

  ok <- tryCatch(
    {
      progress_callback(update)
      TRUE
    },
    error = function(e) {
      .pd_warn(
        "The bootstrap progress callback was disabled after an error: ",
        conditionMessage(e)
      )
      FALSE
    }
  )
  if (isTRUE(ok)) progress_callback else NULL
}

# Plot helpers ---------------------------------------------------------------

#' @noRd
.pd_named_colours <- function(values) {
  values <- sort(unique(as.character(values)))
  stats::setNames(
    grDevices::hcl.colors(length(values), palette = "Dark 3"),
    values
  )
}

#' @noRd
.pd_plot_timevarying <- function(summary_df, bootstrap_success) {
  plot_data <- summary_df
  # Time is a discrete plotting axis. Explicitly making it a factor and
  # dodging along the y direction prevents ggplot2 from treating confidence
  # intervals on the continuous estimate axis as overlapping x intervals.
  plot_data$time <- factor(plot_data$time, levels = unique(plot_data$time))
  colours <- .pd_named_colours(plot_data$covariate)
  dodge <- ggplot2::position_dodge(width = 0.55, orientation = "y")
  title <- if (bootstrap_success > 0L) {
    paste0("Time-specific HTE with bootstrap (B = ", bootstrap_success, ")")
  } else {
    "Time-specific HTE estimates"
  }
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = estimate, y = time, colour = covariate)
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey65") +
    ggplot2::geom_point(position = dodge, size = 2.4) +
    ggplot2::labs(
      x = "Estimate", y = "Time", colour = "Covariate", title = title
    ) +
    ggplot2::scale_colour_manual(values = colours, drop = FALSE) +
    ggplot2::theme_minimal(base_size = 12)
  if (any(is.finite(plot_data$LowerBound))) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(xmin = LowerBound, xmax = UpperBound),
      orientation = "y", width = 0.18,
      position = dodge
    )
  }
  p
}

#' @noRd
.pd_plot_pooled <- function(summary_df, bootstrap_success) {
  plot_data <- summary_df
  colours <- .pd_named_colours(plot_data$term)
  plot_data$term <- factor(plot_data$term, levels = rev(plot_data$term))
  title <- if (bootstrap_success > 0L) {
    paste0("Pooled HTE with bootstrap (B = ", bootstrap_success, ")")
  } else {
    "Pooled HTE estimates"
  }
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = estimate, y = term, colour = term)
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey65") +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::labs(
      x = "Estimate", y = NULL, colour = "Term", title = title
    ) +
    ggplot2::scale_colour_manual(values = colours, drop = FALSE) +
    ggplot2::theme_minimal(base_size = 12)
  if (any(is.finite(plot_data$LowerBound))) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(xmin = LowerBound, xmax = UpperBound),
      orientation = "y", width = 0.18
    )
  }
  p
}

#' Separate estimation of heterogeneous treatment effects by time point
#'
#' Performs separate analyses of heterogeneous treatment effects at each
#' selected time point.
#'
#' `HTESepT()` combines predictions from the propensity score, principal score,
#' and outcome mean models to form the data used for effect estimation. It fits
#' a separate treatment-effect model at each selected time and, when requested,
#' uses bootstrap resampling to calculate confidence intervals.
#'
#' @param data Data prepared by `DataStandard()`.
#' @param ps_fo propensity score model formula
#' @param prin_fo principal score model formula
#' @param out_fo outcome mean model formula
#' @param target_time A non-empty numeric vector containing timepoints of
#'   interest in standardized form. Baseline is allowed.
#' @param B Number of bootstrap replications. Use `0` for point estimates only.
#' @param conf_level The confidence level for Wald intervals calculated from
#'   bootstrap standard errors.
#' @param max_attempts The maximum number of resampling attempts allowed to
#'   obtain `B` successful bootstrap replications. Defaults to `10B`.
#'   Resampling stops once `B` successful replications are obtained or when the
#'   maximum number of attempts is reached, whichever occurs first. Thus,
#'   fewer than `B` successful replications may be returned if the maximum
#'   number of attempts is reached.
#' @param verbose If `TRUE`, print bootstrap progress messages.
#' @param progress_callback An optional function for receiving bootstrap
#'   progress updates. It is called before model fitting, after the point
#'   estimate, after every bootstrap attempt, and when bootstrapping finishes.
#'   Each update is a named list containing
#'   `stage`, `successful`, `requested`, `attempts`, `max_attempts`,
#'   `failed_attempts`, `complete`, `elapsed_seconds`, and `updated_at`.
#'   If the callback produces an error, the function warns once and stops
#'   sending updates; the statistical analysis continues.
#'
#' @return A `pd_hte_timevarying` object containing the estimate for each
#'   requested time, confidence intervals when `B > 0`, model-checking
#'   information, and a summary of successful and failed bootstrap attempts.
#'   Displayed estimates are rounded to three decimal places; `boot_mat` stores
#'   the unrounded bootstrap coefficients.
#' @section Numerical safeguards:
#' Propensity scores are limited to `[0.01, 0.99]`. Their product with estimated
#' survival probabilities under treatment `1` is limited to `[0.005, 0.995]`
#' when effects are estimated. These limits prevent division by probabilities
#' very close to zero, but they can affect the estimates and do not demonstrate
#' adequate overlap or validate the causal assumptions. Review the returned
#' model information, especially when few subjects remain at risk.
#' @inheritSection PDRobust-package Treatment coding
#' @seealso [PDRobust-package], [pd_methods]
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
#' fit <- HTESepT(
#'   pd_dat,
#'   A ~ X1 + X2 + X4,
#'   S ~ X1 + X2 + X4 + A + time,
#'   Y ~ X1 + X2 + A,
#'   target_time = c(0, 2), B = 0
#' )
#' fit$summary
#' }
#' @export
HTESepT <- function(data, ps_fo, prin_fo, out_fo, target_time, B,
                    conf_level = 0.95,
                    max_attempts = NULL,
                    verbose = TRUE,
                    progress_callback = NULL) {
  .pd_require_prepared_data(data, "HTESepT")
  if (length(verbose) != 1L || is.na(verbose) || !is.logical(verbose)) {
    .pd_stop("`verbose` must be TRUE or FALSE.")
  }
  if (length(conf_level) != 1L || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    .pd_stop("`conf_level` must be strictly between 0 and 1.")
  }
  if (length(B) != 1L || !is.finite(B) || B < 0 || B != floor(B)) {
    .pd_stop("`B` must be one nonnegative integer.")
  }
  B <- as.integer(B)
  max_attempts <- max_attempts %||% if (B == 0L) 0L else 10L * B
  if (length(max_attempts) != 1L || !is.finite(max_attempts) ||
      max_attempts < B || max_attempts != floor(max_attempts)) {
    .pd_stop("`max_attempts` must be an integer greater than or equal to `B`.")
  }
  max_attempts <- as.integer(max_attempts)
  progress_callback <- .pd_validate_progress_callback(progress_callback)
  progress_started_at <- Sys.time()
  progress_callback <- .pd_emit_bootstrap_progress(
    progress_callback,
    stage = "initializing",
    successful = 0L,
    requested = B,
    attempts = 0L,
    max_attempts = max_attempts,
    started_at = progress_started_at
  )

  point_capture <- .pd_capture_conditions(
    .pd_htesep_once(data, ps_fo, prin_fo, out_fo, target_time)
  )
  if (inherits(point_capture$value, "error")) {
    .pd_stop(conditionMessage(point_capture$value))
  }
  .pd_emit_analysis_warnings(point_capture$warnings, "HTESepT()")
  original <- point_capture$value
  estimate <- original$estimate
  coefficient_names <- colnames(estimate)
  bootstrap_rows <- list()
  failures <- data.frame(
    attempt = integer(), category = character(), message = character(),
    stringsAsFactors = FALSE
  )
  bootstrap_warnings <- data.frame(
    attempt = integer(), message = character(),
    stringsAsFactors = FALSE
  )
  bootstrap_model_diagnostics <- .pd_empty_model_diagnostics()
  successful <- 0L
  attempts <- 0L
  progress_callback <- .pd_emit_bootstrap_progress(
    progress_callback,
    stage = "bootstrap",
    successful = successful,
    requested = B,
    attempts = attempts,
    max_attempts = max_attempts,
    started_at = progress_started_at
  )

  while (successful < B && attempts < max_attempts) {
    attempts <- attempts + 1L
    boot_data <- .pd_cluster_bootstrap(data)
    captured <- .pd_capture_conditions(
      .pd_htesep_once(
        boot_data, ps_fo, prin_fo, out_fo, target_time
      )
    )
    boot_fit <- captured$value
    if (length(captured$warnings)) {
      bootstrap_warnings <- rbind(
        bootstrap_warnings,
        data.frame(
          attempt = rep.int(attempts, length(captured$warnings)),
          message = captured$warnings,
          stringsAsFactors = FALSE
        )
      )
    }
    if (!inherits(boot_fit, "error")) {
      fit_diagnostics <- .pd_bind_model_diagnostics(
        boot_fit$model_diagnostics,
        analysis = "HTESepT",
        sample = "bootstrap",
        attempt = attempts
      )
      fit_diagnostics <- fit_diagnostics[nzchar(fit_diagnostics$warning), ,
                                         drop = FALSE]
      bootstrap_model_diagnostics <- .pd_bind_model_diagnostics(
        bootstrap_model_diagnostics, fit_diagnostics
      )
    }

    failure_category <- NULL
    failure_message <- NULL
    if (inherits(boot_fit, "error")) {
      failure_message <- conditionMessage(boot_fit)
      failure_category <- .pd_bootstrap_failure_category(failure_message)
    } else if (!identical(dim(boot_fit$estimate), dim(estimate)) ||
               !identical(dimnames(boot_fit$estimate), dimnames(estimate))) {
      failure_category <- "incompatible_estimate"
      failure_message <- "Bootstrap estimate dimensions or names did not match the point estimate."
    } else if (!all(is.finite(boot_fit$estimate))) {
      failure_category <- "invalid_prediction_or_estimate"
      failure_message <- "Bootstrap estimates were missing or non-finite."
    } else if (!all(vapply(
      boot_fit$convergence,
      function(x) isTRUE(x$converged), logical(1)
    ))) {
      failure_category <- "nonconvergence"
      failure_message <- "The binary HTE estimating equation did not converge."
    }
    valid <- is.null(failure_category)

    if (valid) {
      successful <- successful + 1L
      bootstrap_rows[[successful]] <- as.numeric(t(boot_fit$estimate))
    } else {
      failures <- rbind(failures, data.frame(
        attempt = attempts,
        category = failure_category,
        message = failure_message,
        stringsAsFactors = FALSE
      ))
    }
    if (verbose) {
      message(
        "Bootstrap: ", successful, "/", B,
        " successful after ", attempts, " attempts."
      )
    }
    progress_callback <- .pd_emit_bootstrap_progress(
      progress_callback,
      stage = "bootstrap",
      successful = successful,
      requested = B,
      attempts = attempts,
      max_attempts = max_attempts,
      started_at = progress_started_at
    )
  }

  flat_estimate <- as.numeric(t(estimate))
  boot_mat <- if (successful) {
    do.call(rbind, bootstrap_rows)
  } else {
    matrix(numeric(), nrow = 0L, ncol = length(flat_estimate))
  }
  infer <- .pd_bootstrap_summary(flat_estimate, boot_mat, conf_level)
  summary_df <- data.frame(
    time = rep(as.numeric(rownames(estimate)), each = ncol(estimate)),
    covariate = rep(coefficient_names, times = nrow(estimate)),
    estimate = flat_estimate,
    SD = infer$sd,
    LowerBound = infer$lower,
    UpperBound = infer$upper,
    stringsAsFactors = FALSE
  )
  colnames(boot_mat) <- paste(
    summary_df$time, summary_df$covariate, sep = "_"
  )
  if (nrow(boot_mat)) {
    rownames(boot_mat) <- paste0("boot", seq_len(nrow(boot_mat)))
  }
  info <- .pd_make_bootstrap_info(
    B, successful, attempts, failures, bootstrap_warnings,
    bootstrap_model_diagnostics
  )
  if (B > 0L && successful < B) {
    .pd_warn(
      "Only ", successful, " of ", B,
      " bootstrap replications succeeded after ", attempts, " attempts."
    )
  }
  progress_callback <- .pd_emit_bootstrap_progress(
    progress_callback,
    stage = "completed",
    successful = successful,
    requested = B,
    attempts = attempts,
    max_attempts = max_attempts,
    started_at = progress_started_at
  )

  summary_output <- .pd_round_output_columns(
    summary_df, c("estimate", "SD", "LowerBound", "UpperBound")
  )
  model_diagnostics <- .pd_bind_model_diagnostics(
    original$model_diagnostics,
    analysis = "HTESepT",
    sample = "original"
  )
  out <- list(
    summary = summary_output,
    forest_plot = .pd_plot_timevarying(summary_output, successful),
    bootstrap_info = info,
    boot_mat = boot_mat,
    convergence = original$convergence,
    model_diagnostics = model_diagnostics,
    warnings = point_capture$warnings,
    mapping = original$mapping,
    target_time = original$target_time,
    formulas = list(
      propensity = ps_fo, principal = prin_fo, outcome = out_fo
    ),
    settings = list(
      B = B, conf_level = conf_level,
      max_attempts = max_attempts, verbose = verbose
    ),
    call = match.call()
  )
  class(out) <- "pd_hte_timevarying"
  out
}

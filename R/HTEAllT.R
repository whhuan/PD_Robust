#' Estimate pooled heterogeneous treatment effects across all times
#'
#' `HTEAllT()` always uses every actual observed analysis time from the mapped
#' baseline through the mapped cutoff, inclusive. It does not accept or use
#' `target_time`. The propensity, principal-score, and outcome models are
#' refitted internally for the point estimate and for every bootstrap sample.
#' Within one analysis sample, a model fitted to the same rows and formula is
#' reused only to obtain the two counterfactual treatment predictions.
#'
#' Repeated finite-prediction separation or convergence messages are
#' consolidated at the analysis boundary. Model-level details remain available
#' in `model_diagnostics`; bootstrap warnings and their counts are stored in
#' `bootstrap_info`.
#'
#' If the prepared data contain only one analysis time, the estimator omits the
#' time-effect term and records that the time effect is not estimable.
#'
#' @param data A standardized `pd_data` object returned by `DataStandard()`.
#' @param ps_fo Propensity-score formula.
#' @param prin_fo Principal-score formula.
#' @param out_fo Outcome-model formula.
#' @param B Number of successful subject-level bootstrap replications.
#' @param conf_level Confidence level for Wald intervals based on bootstrap SDs.
#' @param max_attempts Maximum bootstrap attempts. `NULL` uses `10 * B`.
#' @param verbose Emit bootstrap progress messages.
#' @param progress_callback Optional function called with one named progress
#'   list before model fitting, after the point estimate, after every bootstrap
#'   attempt, and when bootstrap processing completes. The list contains
#'   `stage`, `successful`, `requested`, `attempts`, `max_attempts`,
#'   `failed_attempts`, `complete`, `elapsed_seconds`, and `updated_at`.
#'   Callback errors warn once and disable further updates without changing the
#'   analysis.
#'
#' @return A `pd_hte_pooled` object. `analysis_times` gives the complete
#'   baseline-to-cutoff grid, `time_effect_estimable` records whether a time
#'   effect was included, and `bootstrap_info` records requested and successful
#'   replicates, attempts, completion status, categorized failures, and captured
#'   warning counts, and model diagnostics. Numeric estimates and interval
#'   summaries are rounded to three decimals only after inference; `boot_mat`
#'   retains full precision.
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
#' fit <- HTEAllT(
#'   pd_dat,
#'   A ~ X1 + X2 + X4,
#'   S ~ X1 + X2 + X4 + A + time,
#'   Y ~ X1 + X2 + A,
#'   B = 0
#' )
#' fit$summary
#' }
#' @export
HTEAllT <- function(data, ps_fo, prin_fo, out_fo, B,
                    conf_level = 0.95,
                    max_attempts = NULL,
                    verbose = TRUE,
                    progress_callback = NULL) {
  .pd_require_prepared_data(data, "HTEAllT")
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
    .pd_hteall_once(data, ps_fo, prin_fo, out_fo)
  )
  if (inherits(point_capture$value, "error")) {
    .pd_stop(conditionMessage(point_capture$value))
  }
  .pd_emit_analysis_warnings(point_capture$warnings, "HTEAllT()")
  original <- point_capture$value
  estimate <- original$estimate
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
      .pd_hteall_once(boot_data, ps_fo, prin_fo, out_fo)
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
        analysis = "HTEAllT",
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
    } else if (length(boot_fit$estimate) != length(estimate) ||
               !identical(names(boot_fit$estimate), names(estimate))) {
      failure_category <- "incompatible_estimate"
      failure_message <- "Bootstrap estimate dimensions or names did not match the point estimate."
    } else if (!all(is.finite(boot_fit$estimate))) {
      failure_category <- "invalid_prediction_or_estimate"
      failure_message <- "Bootstrap estimates were missing or non-finite."
    } else if (!isTRUE(boot_fit$convergence$converged)) {
      failure_category <- "nonconvergence"
      failure_message <- "The binary HTE estimating equation did not converge."
    }
    valid <- is.null(failure_category)

    if (valid) {
      successful <- successful + 1L
      bootstrap_rows[[successful]] <- as.numeric(boot_fit$estimate)
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

  boot_mat <- if (successful) {
    do.call(rbind, bootstrap_rows)
  } else {
    matrix(numeric(), nrow = 0L, ncol = length(estimate))
  }
  infer <- .pd_bootstrap_summary(estimate, boot_mat, conf_level)
  summary_df <- data.frame(
    term = names(estimate),
    estimate = as.numeric(estimate),
    SD = infer$sd,
    LowerBound = infer$lower,
    UpperBound = infer$upper,
    stringsAsFactors = FALSE
  )
  colnames(boot_mat) <- names(estimate)
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
    analysis = "HTEAllT",
    sample = "original"
  )
  out <- list(
    summary = summary_output,
    forest_plot = .pd_plot_pooled(summary_output, successful),
    bootstrap_info = info,
    boot_mat = boot_mat,
    convergence = original$convergence,
    model_diagnostics = model_diagnostics,
    warnings = point_capture$warnings,
    mapping = original$mapping,
    analysis_times = original$analysis_times,
    time_effect_estimable = original$time_effect_estimable,
    note = original$note,
    formulas = list(
      propensity = ps_fo, principal = prin_fo, outcome = out_fo
    ),
    settings = list(
      B = B, conf_level = conf_level,
      max_attempts = max_attempts, verbose = verbose
    ),
    call = match.call()
  )
  class(out) <- "pd_hte_pooled"
  out
}

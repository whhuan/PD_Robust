`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
#' @noRd
.pd_stop <- function(..., call. = FALSE) {
  stop(..., call. = call.)
}

#' @noRd
.pd_warn <- function(..., call. = FALSE) {
  warning(..., call. = call.)
}

#' Round a final user-facing numeric value without changing its structure
#'
#' Integer and logical values are deliberately left unchanged. This helper is
#' used only while constructing public return objects; it must not be called on
#' inputs or intermediate quantities used by an estimator.
#'
#' @noRd
.pd_round_output <- function(x, digits = 3L) {
  if (!is.double(x)) {
    return(x)
  }
  attributes_x <- attributes(x)
  out <- round(x, digits = digits)
  attributes(out) <- attributes_x
  out
}

#' @noRd
.pd_round_output_columns <- function(data, columns, digits = 3L) {
  columns <- intersect(columns, names(data))
  for (column in columns) {
    data[[column]] <- .pd_round_output(data[[column]], digits = digits)
  }
  data
}

#' @noRd
.pd_round_prediction <- function(x, digits = 3L) {
  diagnostics <- attr(x, "pd_model_diagnostics", exact = TRUE)
  out <- .pd_prediction(round(as.numeric(x), digits = digits))
  if (!is.null(diagnostics)) {
    attr(out, "pd_model_diagnostics") <- diagnostics
  }
  out
}

#' @noRd
.pd_as_data_frame <- function(data) {
  mapping <- attr(data, "pd_mapping", exact = TRUE)
  check <- attr(data, "pd_check", exact = TRUE)
  was_pd_data <- inherits(data, "pd_data")
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  } else {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  if (inherits(mapping, "pd_mapping")) attr(data, "pd_mapping") <- mapping
  if (inherits(check, "pd_data_check")) attr(data, "pd_check") <- check
  if (was_pd_data) class(data) <- unique(c("pd_data", class(data)))
  data
}


#' @noRd
.pd_assert_columns <- function(data, columns) {
  missing_columns <- setdiff(unique(columns), names(data))
  if (length(missing_columns)) {
    .pd_stop("Missing required columns: ",
             paste(missing_columns, collapse = ", "), ".")
  }
  invisible(TRUE)
}

#' @noRd
.pd_assert_nonempty <- function(data) {
  if (!nrow(data)) .pd_stop("`data` must contain at least one row.")
  invisible(TRUE)
}

#' @noRd
.pd_key <- function(x) {
  if (is.factor(x)) as.character(x) else as.character(x)
}

#' @noRd
.pd_formula_variables <- function(formula) {
  if (!inherits(formula, "formula")) {
    formula <- stats::as.formula(formula)
  }
  all.vars(formula)
}

#' @noRd
.pd_validate_formula <- function(formula, data, label = "formula") {
  if (is.null(formula)) {
    .pd_stop("`", label, "` cannot be NULL at this stage.")
  }
  if (!inherits(formula, "formula")) {
    formula <- tryCatch(
      stats::as.formula(formula),
      error = function(e) {
        .pd_stop(
          "`", label, "` is not a valid formula: ",
          conditionMessage(e), "."
        )
      }
    )
  }
  missing_vars <- setdiff(.pd_formula_variables(formula), names(data))
  if (length(missing_vars)) {
    .pd_stop(
      "Variables missing from data for `", label, "`: ",
      paste(missing_vars, collapse = ", "), "."
    )
  }
  formula
}

#' @noRd
.pd_solve_binary_score <- function(X, phi_diff, psi_s0, start = NULL,
                                   max_iter = 100L, tolerance = 1e-6) {
  X <- as.matrix(X)
  phi_diff <- as.numeric(phi_diff)
  psi_s0 <- as.numeric(psi_s0)
  p <- ncol(X)

  if (!nrow(X) || !p || !is.numeric(X) ||
      anyNA(X) || any(!is.finite(X))) {
    .pd_stop(
      "The binary HTE design matrix must be nonempty, numeric, and finite."
    )
  }
  if (length(phi_diff) != nrow(X) ||
      anyNA(phi_diff) || any(!is.finite(phi_diff))) {
    .pd_stop(
      "`phi_diff` must be finite and aligned with the binary HTE design matrix."
    )
  }
  if (length(psi_s0) != nrow(X) ||
      anyNA(psi_s0) || any(!is.finite(psi_s0))) {
    .pd_stop(
      "`psi_s0` must be finite and aligned with the binary HTE design matrix."
    )
  }

  # `psi_s0` is the signed estimating quantity from the original HTE
  # equations, not a sampling or regression weight. In particular, untreated
  # subjects who do not survive to cutoff can legitimately have psi_s0 < 0.
  # Neither nonnegativity nor a positive marginal sum is required.
  if (qr(X)$rank < p) {
    .pd_stop(
      "The binary HTE design matrix is rank deficient; coefficients are not estimable."
    )
  }
  start <- if (is.null(start)) rep(0, p) else as.numeric(start)
  if (length(start) != p || anyNA(start) || any(!is.finite(start))) {
    .pd_stop("The starting value must be finite and have one value per coefficient.")
  }

  score <- function(beta) {
    eta <- as.vector(X %*% beta)
    gamma_hat <- 2 * stats::plogis(eta) - 1
    as.vector(crossprod(X, phi_diff - gamma_hat * psi_s0))
  }
  jacobian <- function(beta) {
    eta <- as.vector(X %*% beta)
    probability <- stats::plogis(eta)
    derivative_weight <- 2 * probability * (1 - probability) * psi_s0
    -crossprod(X, X * derivative_weight)
  }

  warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      rootSolve::multiroot(
        f = score,
        jacfunc = jacobian,
        start = start,
        maxiter = as.integer(max_iter),
        rtol = tolerance
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      .pd_stop(
        "The binary HTE estimating equation failed: ",
        conditionMessage(e), "."
      )
    }
  )
  if (length(warnings)) {
    .pd_warn(
      "The binary HTE estimating equation produced a fitting warning: ",
      paste(unique(warnings), collapse = "; "), "."
    )
  }
  finite_root <- length(fit$root) == p &&
    all(is.finite(fit$root))
  precision_ok <- is.finite(fit$estim.precis) &&
    fit$estim.precis <= sqrt(tolerance)
  converged <- finite_root && is.finite(fit$estim.precis) &&
    (fit$iter < as.integer(max_iter) || precision_ok)
  if (!finite_root) {
    .pd_stop(
      "The binary HTE estimating equation did not produce a finite coefficient vector."
    )
  }
  if (!converged) {
    .pd_warn("`rootSolve::multiroot()` did not converge within `max_iter`.")
  }
  list(
    root = as.numeric(fit$root),
    converged = converged,
    iterations = as.integer(fit$iter),
    precision = fit$estim.precis,
    warnings = unique(warnings),
    solver = "rootSolve::multiroot"
  )
}


#' @noRd
.pd_prediction <- function(x) {
  structure(as.numeric(x), class = c("pd_prediction", "numeric"))
}

#' @noRd
.pd_structural_outcome <- function(y, required_observed, context = "analysis") {
  bad <- required_observed & is.na(y)
  if (any(bad)) {
    .pd_stop(
      "Outcome values are missing for ", sum(bad),
      " observations that must be observed in ", context, "."
    )
  }
  ifelse(required_observed, y, 0)
}

#' @noRd
.pd_cluster_bootstrap <- function(data) {
  mapping <- .pd_mapping_for_data(data, "internal cluster bootstrap")
  id_col <- mapping$id_col
  ids <- unique(.pd_key(data[[id_col]]))
  sampled <- sample(ids, size = length(ids), replace = TRUE)
  pieces <- vector("list", length(sampled))
  key <- .pd_key(data[[id_col]])
  for (i in seq_along(sampled)) {
    piece <- data[key == sampled[i], , drop = FALSE]
    piece[[id_col]] <- i
    pieces[[i]] <- piece
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  attr(out, "pd_mapping") <- mapping
  class(out) <- unique(c("pd_data", class(out)))
  out
}

#' Capture warnings without converting a usable bootstrap result into a failure
#'
#' @noRd
.pd_capture_conditions <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      force(expr),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  list(value = value, warnings = unique(warnings))
}

#' Consolidate captured warnings at a public analysis boundary
#'
#' Internal model fits retain their own diagnostics, while this helper prevents
#' the same low-level message from being printed once per treatment arm, time
#' point, or sensitivity scenario.
#'
#' @noRd
.pd_emit_analysis_warnings <- function(warnings, analysis) {
  warnings <- unique(warnings[nzchar(warnings)])
  if (!length(warnings)) {
    return(invisible(character()))
  }

  model_warning <- grepl(
    "^Model fitting warning for `", warnings, fixed = FALSE
  )
  if (any(model_warning)) {
    .pd_warn(
      analysis, " encountered nuisance-model instability in ",
      sum(model_warning), " distinct model warning",
      if (sum(model_warning) == 1L) "" else "s",
      ". Finite prediction-based fits were retained where permitted. ",
      paste(warnings[model_warning], collapse = " ")
    )
  }
  if (any(!model_warning)) {
    for (message in warnings[!model_warning]) {
      .pd_warn(message)
    }
  }
  invisible(warnings)
}

#' @noRd
.pd_bootstrap_failure_category <- function(message) {
  message <- tolower(paste(message, collapse = " "))
  if (grepl("rank deficient|singular|not estimable|ill-conditioned", message)) {
    return("nonestimable_system")
  }
  if (grepl("converg|steady-state|multiroot", message)) {
    return("nonconvergence")
  }
  if (grepl("both 0 and 1|one observed level|no at-risk|no nonmissing|no observations", message)) {
    return("degenerate_resample")
  }
  if (grepl("predict|non-finite|nonfinite|missing|misaligned", message)) {
    return("invalid_prediction_or_estimate")
  }
  if (grepl("model fitting|model matrix|contrasts", message)) {
    return("nuisance_model_fit")
  }
  "other"
}

#' @noRd
.pd_bootstrap_summary <- function(est, boot_mat, conf_level) {
  est <- as.numeric(est)
  if (is.null(boot_mat) || !nrow(boot_mat)) {
    return(list(
      sd = rep(NA_real_, length(est)),
      lower = rep(NA_real_, length(est)),
      upper = rep(NA_real_, length(est))
    ))
  }
  sd_value <- apply(boot_mat, 2L, stats::sd, na.rm = TRUE)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  list(
    sd = sd_value,
    lower = est - z * sd_value,
    upper = est + z * sd_value
  )
}

#' @noRd
.pd_make_bootstrap_info <- function(requested, successful, attempts, failures,
                                    warnings = NULL,
                                    model_diagnostics = NULL) {
  if (is.null(warnings)) {
    warnings <- data.frame(
      attempt = integer(), message = character(),
      stringsAsFactors = FALSE
    )
  }
  failure_counts <- if (nrow(failures)) {
    counts <- table(failures$category)
    data.frame(
      category = names(counts),
      count = as.integer(counts),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      category = character(), count = integer(),
      stringsAsFactors = FALSE
    )
  }
  warning_counts <- if (nrow(warnings)) {
    counts <- table(warnings$message)
    data.frame(
      message = names(counts),
      count = as.integer(counts),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      message = character(), count = integer(),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(model_diagnostics)) {
    model_diagnostics <- .pd_empty_model_diagnostics()
  }
  list(
    requested = as.integer(requested),
    successful = as.integer(successful),
    attempts = as.integer(attempts),
    complete = identical(as.integer(successful), as.integer(requested)),
    failures = failures,
    failure_counts = failure_counts,
    warnings = warnings,
    warning_counts = warning_counts,
    model_diagnostics = model_diagnostics
  )
}

#' @noRd
.pd_time_label <- function(x) {
  format(x, trim = TRUE, scientific = FALSE)
}

# Model preflight and checked fitting ----------------------------------------

#' @noRd
.pd_model_preflight <- function(formula, data, label,
                                family = c("gaussian", "binomial"),
                                allow_aliased = FALSE) {
  family <- match.arg(family)
  formula <- .pd_validate_formula(formula, data, label)
  terms_object <- stats::terms(formula, data = data)
  if (attr(terms_object, "response") != 1L) {
    .pd_stop("`", label, "` must be a two-sided formula with one response.")
  }

  model_frame <- tryCatch(
    stats::model.frame(
      formula, data = data, na.action = stats::na.pass,
      drop.unused.levels = TRUE
    ),
    error = function(e) {
      .pd_stop(
        "Model preflight failed for `", label, "`: ",
        conditionMessage(e), "."
      )
    }
  )
  complete <- stats::complete.cases(model_frame)
  model_frame <- model_frame[complete, , drop = FALSE]
  if (!nrow(model_frame)) {
    .pd_stop("No complete cases are available for `", label, "`.")
  }

  design <- tryCatch(
    stats::model.matrix(terms_object, data = model_frame),
    error = function(e) {
      .pd_stop(
        "The model matrix for `", label, "` is invalid: ",
        conditionMessage(e), "."
      )
    }
  )
  if (anyNA(design) || any(!is.finite(design))) {
    .pd_stop("The model matrix for `", label, "` contains missing or non-finite values.")
  }

  design_rank <- qr(design)$rank
  if (!isTRUE(allow_aliased) && design_rank < ncol(design)) {
    .pd_stop(
      "The model matrix for `", label,
      "` is rank deficient; remove aliased or collinear terms."
    )
  }
  if (nrow(design) <= ncol(design)) {
    .pd_stop(
      "Insufficient complete cases for `", label, "`: ",
      nrow(design), " complete rows for ", ncol(design),
      " model coefficients."
    )
  }

  response <- stats::model.response(model_frame)
  if (!is.numeric(response) && !is.logical(response)) {
    .pd_stop("The response for `", label, "` must be numeric.")
  }
  response <- as.numeric(response)
  if (any(!is.finite(response))) {
    .pd_stop("The response for `", label, "` contains non-finite values.")
  }
  if (family == "binomial") {
    observed <- sort(unique(response))
    if (!all(observed %in% c(0, 1)) || length(observed) < 2L) {
      .pd_stop("The response for `", label, "` must contain both 0 and 1.")
    }
  } else if (length(unique(response)) < 2L) {
    .pd_stop("The outcome for `", label, "` has no variation.")
  }

  list(
    formula = formula,
    data = data[complete, , drop = FALSE],
    model_frame = model_frame,
    design = design,
    response = response,
    complete = complete,
    rank = design_rank,
    aliased = design_rank < ncol(design)
  )
}

#' Remove predictors that are fixed by a model's fitting design
#'
#' This helper is intentionally conservative: only variables explicitly named
#' by the caller are eligible for removal, and they are removed only when they
#' are constant in the actual complete-case fitting data. Terms involving a
#' fixed variable (including interactions) are removed together.
#'
#' @noRd
.pd_exclude_design_fixed_predictors <- function(formula, data, fixed, label) {
  formula <- .pd_validate_formula(formula, data, label)
  variables <- .pd_formula_variables(formula)
  response <- variables[1L]
  eligible <- intersect(setdiff(variables, response), fixed)
  if (!length(eligible)) {
    return(formula)
  }

  complete <- stats::complete.cases(data[, variables, drop = FALSE])
  fitting_data <- data[complete, , drop = FALSE]
  if (!nrow(fitting_data)) {
    return(formula)
  }
  fixed_in_data <- eligible[vapply(
    eligible,
    function(variable) {
      length(unique(fitting_data[[variable]])) < 2L
    },
    logical(1)
  )]
  if (!length(fixed_in_data)) {
    return(formula)
  }

  terms_object <- stats::terms(formula, data = fitting_data)
  term_labels <- attr(terms_object, "term.labels")
  drop_terms <- which(vapply(
    term_labels,
    function(term_label) {
      any(all.vars(stats::as.formula(paste("~", term_label))) %in%
            fixed_in_data)
    },
    logical(1)
  ))
  if (!length(drop_terms)) {
    return(formula)
  }

  reduced_terms <- stats::drop.terms(
    terms_object, dropx = drop_terms, keep.response = TRUE
  )
  reduced_formula <- stats::formula(reduced_terms)
  environment(reduced_formula) <- environment(formula)
  reduced_formula
}

#' @noRd
.pd_model_warning_message <- function(label, warnings) {
  paste0(
    "Model fitting warning for `", label, "`: ",
    paste(unique(warnings), collapse = "; "), "."
  )
}

#' @noRd
.pd_empty_model_diagnostics <- function() {
  data.frame(
    label = character(),
    analysis = character(),
    sample = character(),
    attempt = integer(),
    target_time = numeric(),
    treatment = numeric(),
    n_rows = integer(),
    n_subjects = integer(),
    response_0 = integer(),
    response_1 = integer(),
    formula = character(),
    predictors = character(),
    rank_deficient = logical(),
    predictions_finite = logical(),
    converged = logical(),
    separation = logical(),
    warning = character(),
    stringsAsFactors = FALSE
  )
}

#' @noRd
.pd_model_diagnostic <- function(fit, formula, data, label, warnings,
                                 separated = FALSE,
                                 diagnostic_context = list()) {
  model_matrix <- tryCatch(
    stats::model.matrix(fit),
    error = function(e) NULL
  )
  response <- tryCatch(
    stats::model.response(stats::model.frame(fit)),
    error = function(e) numeric()
  )
  response <- as.numeric(response)
  response_counts <- table(factor(response, levels = c(0, 1)))
  predictors <- setdiff(.pd_formula_variables(formula),
                        .pd_formula_variables(formula)[1L])
  context_value <- function(name, default) {
    value <- diagnostic_context[[name]]
    if (is.null(value) || !length(value)) default else value[[1L]]
  }

  data.frame(
    label = label,
    analysis = as.character(context_value("analysis", NA_character_)),
    sample = as.character(context_value("sample", NA_character_)),
    attempt = as.integer(context_value("attempt", NA_integer_)),
    target_time = as.numeric(context_value("target_time", NA_real_)),
    treatment = as.numeric(context_value("treatment", NA_real_)),
    n_rows = as.integer(stats::nobs(fit)),
    n_subjects = as.integer(context_value("n_subjects", NA_integer_)),
    response_0 = as.integer(response_counts[[1L]]),
    response_1 = as.integer(response_counts[[2L]]),
    formula = paste(deparse(formula, width.cutoff = 500L), collapse = " "),
    predictors = paste(predictors, collapse = ", "),
    rank_deficient = !is.null(model_matrix) &&
      qr(model_matrix)$rank < ncol(model_matrix),
    predictions_finite = {
      fitted <- stats::fitted(fit)
      length(fitted) > 0L && all(is.finite(fitted))
    },
    converged = isTRUE(fit$converged %||% TRUE),
    separation = isTRUE(separated),
    warning = paste(unique(warnings), collapse = "; "),
    stringsAsFactors = FALSE
  )
}

#' @noRd
.pd_model_diagnostics <- function(x) {
  value <- attr(x, "pd_model_diagnostics", exact = TRUE)
  if (is.data.frame(value)) value else .pd_empty_model_diagnostics()
}

#' @noRd
.pd_bind_model_diagnostics <- function(..., analysis = NULL, sample = NULL,
                                       attempt = NULL) {
  values <- list(...)
  values <- values[vapply(values, is.data.frame, logical(1))]
  values <- values[vapply(values, nrow, integer(1)) > 0L]
  if (!length(values)) {
    return(.pd_empty_model_diagnostics())
  }
  out <- do.call(rbind, values)
  rownames(out) <- NULL
  if (!is.null(analysis)) out$analysis <- as.character(analysis)
  if (!is.null(sample)) out$sample <- as.character(sample)
  if (!is.null(attempt)) out$attempt <- as.integer(attempt)
  out
}

#' @noRd
.pd_fit_glm_checked <- function(formula, data, label,
                                allow_aliased = FALSE,
                                strict = TRUE,
                                context_warnings = character(),
                                diagnostic_context = list(), ...) {
  if (isTRUE(strict)) {
    preflight <- .pd_model_preflight(
      formula, data, label, family = "binomial",
      allow_aliased = allow_aliased
    )
    fit_formula <- preflight$formula
    fit_data <- preflight$data
  } else {
    fit_formula <- .pd_validate_formula(formula, data, label)
    fit_data <- data
  }
  warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      stats::glm(
        fit_formula,
        data = fit_data,
        family = stats::binomial(link = "logit"),
        ...
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      .pd_stop(
        "Model fitting failed for `", label, "`: ",
        conditionMessage(e), "."
      )
    }
  )
  diagnostic_warnings <- c(context_warnings, warnings)
  convergence_reported <- any(grepl(
    "did not converge|algorithm did not converge",
    diagnostic_warnings,
    ignore.case = TRUE
  ))
  coefficients <- stats::coef(fit)
  if (any(is.infinite(coefficients), na.rm = TRUE)) {
    .pd_stop(
      "The logistic model for `", label,
      "` has infinite coefficients."
    )
  }
  if (isTRUE(strict) && !isTRUE(allow_aliased) &&
      (anyNA(coefficients) || any(!is.finite(coefficients)))) {
    .pd_stop(
      "The logistic model for `", label,
      "` has non-estimable or non-finite coefficients."
    )
  }
  fitted <- stats::fitted(fit)
  if (!length(fitted) || anyNA(fitted) || any(!is.finite(fitted))) {
    .pd_stop(
      "The logistic model for `", label,
      "` did not produce usable finite fitted probabilities."
    )
  }
  finite_coefficients <- coefficients[is.finite(coefficients)]
  fitted_probability_warning <- any(grepl(
    "fitted probabilities numerically 0 or 1",
    diagnostic_warnings,
    ignore.case = TRUE
  ))
  separated <- fitted_probability_warning ||
    any(abs(finite_coefficients) > 25) ||
    any(fitted < 1e-8 | fitted > 1 - 1e-8)
  diagnostic_warnings <- diagnostic_warnings[!grepl(
    "algorithm did not converge|fitted probabilities numerically 0 or 1",
    diagnostic_warnings,
    ignore.case = TRUE
  )]
  if (!isTRUE(fit$converged) || convergence_reported) {
    diagnostic_warnings <- c(
      diagnostic_warnings,
      "the logistic model did not converge"
    )
  }
  if (separated) {
    diagnostic_warnings <- c(
      diagnostic_warnings,
      paste0(
        "the logistic model shows complete or quasi-complete separation; ",
        "finite predictions are retained, but coefficient-based ",
        "interpretation may be unstable"
      )
    )
  }
  diagnostic_warnings <- unique(diagnostic_warnings)
  if (length(diagnostic_warnings)) {
    .pd_warn(.pd_model_warning_message(label, diagnostic_warnings))
  }
  attr(fit, "pd_warnings") <- diagnostic_warnings
  attr(fit, "pd_model_diagnostics") <- .pd_model_diagnostic(
    fit = fit,
    formula = fit_formula,
    data = fit_data,
    label = label,
    warnings = diagnostic_warnings,
    separated = separated,
    diagnostic_context = diagnostic_context
  )
  fit
}

#' @noRd
.pd_fit_lm_checked <- function(formula, data, label,
                               allow_aliased = FALSE,
                               strict = TRUE,
                               diagnostic_context = list(), ...) {
  if (isTRUE(strict)) {
    preflight <- .pd_model_preflight(
      formula, data, label, family = "gaussian",
      allow_aliased = allow_aliased
    )
    fit_formula <- preflight$formula
    fit_data <- preflight$data
  } else {
    fit_formula <- .pd_validate_formula(formula, data, label)
    fit_data <- data
  }
  warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      stats::lm(fit_formula, data = fit_data, ...),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      .pd_stop(
        "Model fitting failed for `", label, "`: ",
        conditionMessage(e), "."
      )
    }
  )
  if (length(warnings)) {
    .pd_warn(.pd_model_warning_message(label, warnings))
  }
  coefficients <- stats::coef(fit)
  if (any(is.infinite(coefficients), na.rm = TRUE)) {
    .pd_stop(
      "The linear model for `", label,
      "` has infinite coefficients."
    )
  }
  if (isTRUE(strict) && !isTRUE(allow_aliased) &&
      (anyNA(coefficients) || any(!is.finite(coefficients)))) {
    .pd_stop(
      "The linear model for `", label,
      "` has non-estimable or non-finite coefficients."
    )
  }
  attr(fit, "pd_warnings") <- unique(warnings)
  attr(fit, "pd_model_diagnostics") <- .pd_model_diagnostic(
    fit = fit,
    formula = fit_formula,
    data = fit_data,
    label = label,
    warnings = unique(warnings),
    separated = FALSE,
    diagnostic_context = diagnostic_context
  )
  fit
}

#' @noRd
.pd_predict_checked <- function(fit, newdata, label,
                                allow_rank_deficient = FALSE) {
  warnings <- character()
  prediction <- tryCatch(
    withCallingHandlers(
      stats::predict(fit, newdata = newdata, type = "response"),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      .pd_stop(
        "Prediction failed for `", label, "`: ",
        conditionMessage(e), "."
      )
    }
  )
  if (length(prediction) != nrow(newdata) ||
      anyNA(prediction) || any(!is.finite(prediction))) {
    .pd_stop(
      "Predictions for `", label,
      "` are missing, non-finite, or misaligned."
    )
  }
  report <- warnings
  if (isTRUE(allow_rank_deficient) && length(report)) {
    report <- report[!grepl(
      "rank[- ]deficient|rank deficient",
      report,
      ignore.case = TRUE
    )]
  }
  if (length(report)) {
    .pd_warn(
      "Prediction warning for `", label, "`: ",
      paste(unique(report), collapse = "; "), "."
    )
  }
  as.numeric(prediction)
}

#' @noRd
.pd_fit_rq_checked <- function(formula, data, weights, tau, label) {
  preflight <- .pd_model_preflight(
    formula, data, label, family = "gaussian"
  )
  weights <- as.numeric(weights)
  weights <- weights[preflight$complete]
  if (length(weights) != nrow(preflight$data) ||
      anyNA(weights) || any(!is.finite(weights)) || any(weights < 0) ||
      sum(weights) <= 0) {
    .pd_stop(
      "Weights for `", label,
      "` must be finite, nonnegative, aligned, and have a positive sum."
    )
  }
  preflight$data$.pd_weights <- weights
  warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      quantreg::rq(
        preflight$formula,
        data = preflight$data,
        weights = .pd_weights,
        tau = tau,
        method = "br",
        model = TRUE
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      .pd_stop(
        "Weighted quantile regression failed for `", label, "`: ",
        conditionMessage(e), "."
      )
    }
  )
  if (length(warnings)) {
    .pd_warn(.pd_model_warning_message(label, warnings))
  }
  coefficients <- stats::coef(fit)
  if (!length(coefficients) || anyNA(coefficients) ||
      any(!is.finite(coefficients))) {
    .pd_stop(
      "Weighted quantile regression for `", label,
      "` produced non-estimable coefficients."
    )
  }
  fit
}

#' @noRd
.pd_solve_checked <- function(lhs, rhs, label) {
  lhs <- as.matrix(lhs)
  rhs <- as.numeric(rhs)
  if (!nrow(lhs) || nrow(lhs) != ncol(lhs) ||
      length(rhs) != nrow(lhs) ||
      anyNA(lhs) || any(!is.finite(lhs)) ||
      anyNA(rhs) || any(!is.finite(rhs))) {
    .pd_stop("The estimating system for ", label, " is invalid or non-finite.")
  }
  if (qr(lhs)$rank < ncol(lhs)) {
    .pd_stop(
      "The estimating system for ", label,
      " is rank deficient; the requested coefficients are not estimable."
    )
  }
  condition_number <- kappa(lhs, exact = TRUE)
  if (!is.finite(condition_number) || condition_number > 1e12) {
    .pd_warn(
      "The estimating system for ", label,
      " is ill-conditioned; finite estimates will be retained if solving succeeds."
    )
  }
  value <- tryCatch(
    drop(solve(lhs, rhs)),
    error = function(e) {
      .pd_stop(
        "The estimating system for ", label,
        " could not be solved: ", conditionMessage(e), "."
      )
    }
  )
  if (length(value) != length(rhs) || anyNA(value) ||
      any(!is.finite(value))) {
    .pd_stop(
      "The estimating system for ", label,
      " did not produce finite coefficients."
    )
  }
  value
}

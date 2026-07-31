# Internal implementation. Public interfaces are documented beside their function definitions.

.pd_display_values <- function(x, max_values = 12L) {
  values <- unique(stats::na.omit(if (is.factor(x)) as.character(x) else x))
  if (!length(values)) return("<none>")
  labels <- as.character(values)
  if (length(labels) > max_values) {
    labels <- c(labels[seq_len(max_values)], "...")
  }
  paste(labels, collapse = ", ")
}

.pd_binary_conversion <- function(x) {
  original_class <- paste(class(x), collapse = "/")
  if (is.logical(x)) {
    return(list(
      ok = TRUE, canonical = FALSE, value = as.integer(x),
      original_class = original_class, invalid = character(),
      invalid_rows = integer()
    ))
  }
  if (is.numeric(x) || is.integer(x)) {
    invalid_rows <- which(!is.na(x) & !x %in% c(0, 1))
    invalid <- unique(x[invalid_rows])
    return(list(
      ok = !length(invalid), canonical = !length(invalid),
      value = if (!length(invalid)) as.integer(x) else NULL,
      original_class = original_class, invalid = as.character(invalid),
      invalid_rows = invalid_rows
    ))
  }
  if (is.character(x) || is.factor(x)) {
    labels <- if (is.factor(x)) as.character(x) else x
    nonmissing <- stats::na.omit(labels)
    invalid <- unique(nonmissing[!nonmissing %in% c("0", "1")])
    invalid_rows <- which(!is.na(labels) & !labels %in% c("0", "1"))
    value <- if (!length(invalid)) {
      out <- rep(NA_integer_, length(labels))
      out[!is.na(labels)] <- as.integer(labels[!is.na(labels)])
      out
    } else {
      NULL
    }
    return(list(
      ok = !length(invalid), canonical = FALSE, value = value,
      original_class = original_class, invalid = invalid,
      invalid_rows = invalid_rows
    ))
  }
  list(
    ok = FALSE, canonical = FALSE, value = NULL,
    original_class = original_class,
    invalid = as.character(unique(stats::na.omit(x))),
    invalid_rows = which(!is.na(x))
  )
}

.pd_numeric_conversion <- function(x) {
  original_class <- paste(class(x), collapse = "/")
  if (is.numeric(x) || is.integer(x)) {
    value <- as.numeric(x)
    invalid <- which(!is.na(value) & !is.finite(value))
    return(list(
      ok = !length(invalid), canonical = !length(invalid), value = value,
      original_class = original_class, invalid_rows = invalid
    ))
  }
  if (is.character(x) || is.factor(x)) {
    labels <- if (is.factor(x)) as.character(x) else x
    value <- suppressWarnings(as.numeric(labels))
    invalid <- which(!is.na(labels) & (is.na(value) | !is.finite(value)))
    return(list(
      ok = !length(invalid), canonical = FALSE,
      value = if (!length(invalid)) value else NULL,
      original_class = original_class, invalid_rows = invalid
    ))
  }
  list(
    ok = FALSE, canonical = FALSE, value = NULL,
    original_class = original_class,
    invalid_rows = seq_along(x)
  )
}

.pd_is_consecutive_integer_id <- function(x) {
  if (!(is.integer(x) || is.numeric(x)) || anyNA(x)) return(FALSE)
  values <- sort(unique(as.numeric(x)))
  isTRUE(all.equal(values, as.numeric(seq_along(values))))
}

.pd_near_zero_variation <- function(x) {
  values <- stats::na.omit(x)
  if (!length(values)) return(TRUE)
  counts <- sort(table(values), decreasing = TRUE)
  if (length(counts) <= 1L) return(TRUE)
  frequency_ratio <- as.numeric(counts[1L] / counts[2L])
  percent_unique <- 100 * length(counts) / length(values)
  frequency_ratio >= 19 && percent_unique <= 10
}

.pd_check_data_impl <- function(data, mapping, strict = FALSE) {
  mapping <- .pd_validate_mapping(mapping)
  if (length(strict) != 1L || is.na(strict) || !is.logical(strict)) {
    .pd_stop("`strict` must be TRUE or FALSE.")
  }
  baseline_time <- mapping$baseline_time
  cutoff_time <- mapping$cutoff_time
  covariates <- mapping$covariates
  outcome_type <- mapping$y_type
  id_col <- mapping$id_col
  time_col <- mapping$time_col
  A_col <- mapping$A_col
  S_col <- mapping$S_col
  Y_col <- mapping$Y_col
  data <- .pd_as_data_frame(data)
  checks <- list()
  diagnostics <- list()

  add_check <- function(check, passed, severity, details, recommendation = "",
                        standardize_can_fix = FALSE,
                        requires_manual_resolution = FALSE,
                        analysis_blocking = severity == "error") {
    checks[[length(checks) + 1L]] <<- data.frame(
      check = check,
      passed = isTRUE(passed),
      severity = severity,
      standardize_can_fix = isTRUE(standardize_can_fix),
      requires_manual_resolution = isTRUE(requires_manual_resolution),
      analysis_blocking = isTRUE(analysis_blocking),
      details = as.character(details),
      recommendation = as.character(recommendation),
      stringsAsFactors = FALSE
    )
  }

  structural_columns <- c(id_col, time_col, A_col, S_col, Y_col)
  required_columns <- unique(c(structural_columns, covariates))
  missing_columns <- setdiff(required_columns, names(data))
  add_check(
    "required_columns",
    !length(missing_columns),
    "error",
    if (length(missing_columns)) {
      paste("Missing:", paste(missing_columns, collapse = ", "))
    } else {
      paste(length(required_columns), "required columns are present.")
    },
    "Correct the mapping or add/rename the missing columns manually.",
    requires_manual_resolution = length(missing_columns) > 0L
  )

  if (length(missing_columns)) {
    checks_df <- do.call(rbind, checks)
    report <- structure(
      list(
        valid = FALSE,
        ready_for_analysis = FALSE,
        manual_resolution_required = TRUE,
        can_standardize = FALSE,
        checks = checks_df,
        settings = list(mapping = mapping),
        diagnostics = diagnostics
      ),
      class = "pd_data_check"
    )
    if (strict) .pd_stop("Data validation failed: required columns are missing.")
    return(report)
  }

  add_check(
    "nonempty_data",
    nrow(data) > 0L,
    "error",
    paste(nrow(data), "rows detected."),
    "Supply a nonempty long-format data set.",
    requires_manual_resolution = nrow(data) == 0L
  )
  if (!nrow(data)) {
    checks_df <- do.call(rbind, checks)
    report <- structure(
      list(
        valid = FALSE, ready_for_analysis = FALSE,
        manual_resolution_required = TRUE, can_standardize = FALSE,
        checks = checks_df,
        settings = list(mapping = mapping),
        diagnostics = diagnostics
      ),
      class = "pd_data_check"
    )
    if (strict) .pd_stop("Data validation failed: data are empty.")
    return(report)
  }

  missing_id_time_rows <- which(
    is.na(data[[id_col]]) | is.na(data[[time_col]])
  )
  diagnostics$missing_id_time_rows <- missing_id_time_rows
  add_check(
    "missing_id_or_time",
    !length(missing_id_time_rows),
    "error",
    if (length(missing_id_time_rows)) {
      paste(
        length(missing_id_time_rows), "rows have missing ID/time; original rows:",
        paste(missing_id_time_rows, collapse = ", ")
      )
    } else {
      "No rows have missing ID or time."
    },
    "Restore the identifiers/time values, or use `drop = TRUE` to remove unidentifiable rows.",
    standardize_can_fix = length(missing_id_time_rows) > 0L,
    requires_manual_resolution = FALSE
  )

  time_conversion <- .pd_numeric_conversion(data[[time_col]])
  add_check(
    "time_encoding",
    time_conversion$ok && time_conversion$canonical,
    if (time_conversion$ok) "warning" else "error",
    paste(
      "Time class:", time_conversion$original_class,
      if (time_conversion$ok) {
        "; values can be ordered numerically."
      } else {
        paste0("; invalid rows: ", paste(time_conversion$invalid_rows, collapse = ", "), ".")
      }
    ),
    if (time_conversion$ok) {
      "Standardization will map required raw times to internal integers 0, 1, ..., n."
    } else {
      "Correct the time coding manually; every nonmissing time must be unambiguously numeric."
    },
    standardize_can_fix = time_conversion$ok && !time_conversion$canonical,
    requires_manual_resolution = !time_conversion$ok,
    analysis_blocking = !(time_conversion$ok && time_conversion$canonical)
  )
  numeric_time <- if (time_conversion$ok) time_conversion$value else
    rep(NA_real_, nrow(data))
  observed_times <- sort(unique(stats::na.omit(numeric_time)))

  endpoints_ok <- baseline_time %in% observed_times &&
    cutoff_time %in% observed_times && baseline_time <= cutoff_time
  add_check(
    "mapping_time_endpoints",
    endpoints_ok,
    "error",
    paste(
      "baseline_time =", .pd_time_label(baseline_time),
      "; cutoff_time =", .pd_time_label(cutoff_time),
      "; observed times =", paste(.pd_time_label(observed_times), collapse = ", "), "."
    ),
    "Correct the mapping or the underlying time coding before analysis.",
    requires_manual_resolution = !endpoints_ok
  )

  observed_in_window <- observed_times[
    observed_times >= baseline_time & observed_times <= cutoff_time
  ]
  required_times <- as.numeric(observed_in_window)
  analysis_grid_ok <- length(required_times) > 0L &&
    baseline_time %in% required_times && cutoff_time %in% required_times
  add_check(
    "analysis_time_grid",
    analysis_grid_ok,
    "error",
    if (analysis_grid_ok) {
      paste(
        "All actual observed times from baseline through cutoff are included:",
        paste(.pd_time_label(required_times), collapse = ", "), "."
      )
    } else {
      paste(
        "The observed baseline-to-cutoff analysis grid is incomplete. Observed times:",
        paste(.pd_time_label(observed_times), collapse = ", "), "."
      )
    },
    "Correct the mapped endpoints or underlying time records. All observed visits within the mapped window are retained.",
    requires_manual_resolution = !analysis_grid_ok
  )
  analysis_times <- required_times

  complete_rows <- which(!is.na(data[[id_col]]) & !is.na(numeric_time))
  pair_key <- paste(
    .pd_key(data[[id_col]][complete_rows]),
    numeric_time[complete_rows],
    sep = "\r"
  )
  duplicate_local <- duplicated(pair_key) | duplicated(pair_key, fromLast = TRUE)
  duplicate_rows <- complete_rows[duplicate_local]
  duplicate_subjects <- unique(.pd_key(data[[id_col]][duplicate_rows]))
  diagnostics$duplicate_rows <- duplicate_rows
  diagnostics$duplicate_subjects <- duplicate_subjects
  add_check(
    "duplicate_id_time_records",
    !length(duplicate_rows),
    "error",
    if (length(duplicate_rows)) {
      paste(
        length(duplicate_rows), "rows across", length(duplicate_subjects),
        "subjects belong to duplicated ID-time pairs; original rows:",
        paste(duplicate_rows, collapse = ", "), "."
      )
    } else {
      "No duplicated ID-time pairs were found."
    },
    "Manually choose an aggregation or record-selection rule; duplicates are never silently retained.",
    requires_manual_resolution = length(duplicate_rows) > 0L
  )

  valid_id_rows <- which(!is.na(data[[id_col]]))
  id_values <- unique(.pd_key(data[[id_col]][valid_id_rows]))
  id_groups <- split(valid_id_rows, .pd_key(data[[id_col]][valid_id_rows]))
  complete_subject <- stats::setNames(rep(FALSE, length(id_values)), id_values)
  missing_by_time <- data.frame(
    time = analysis_times,
    missing_subjects = integer(length(analysis_times)),
    stringsAsFactors = FALSE
  )
  if (length(analysis_times) && length(id_groups)) {
    for (subject in names(id_groups)) {
      observed <- unique(numeric_time[id_groups[[subject]]])
      complete_subject[[subject]] <- all(analysis_times %in% observed)
    }
    missing_by_time$missing_subjects <- vapply(analysis_times, function(t) {
      sum(!vapply(id_groups, function(idx) t %in% numeric_time[idx], logical(1)))
    }, integer(1))
  }
  diagnostics$missing_by_time <- missing_by_time
  diagnostics$incomplete_subjects <- names(complete_subject)[!complete_subject]
  n_subjects <- length(id_groups)
  n_complete <- sum(complete_subject)
  add_check(
    "complete_longitudinal_structure",
    analysis_grid_ok && n_subjects > 0L && n_complete == n_subjects,
    "error",
    paste(
      n_complete, "of", n_subjects, "subjects (",
      if (n_subjects) round(100 * n_complete / n_subjects, 3) else 0,
      "%) have exactly one record at every required time;",
      n_subjects - n_complete, "are missing at least one visit. Missing counts by time:",
      if (nrow(missing_by_time)) {
        paste(
          paste0(.pd_time_label(missing_by_time$time), "=",
                 missing_by_time$missing_subjects),
          collapse = ", "
        )
      } else {
        "<unavailable>"
      },
      "."
    ),
    "Recover missing records, shorten the mapped window, or use `drop = TRUE` for an explicitly reported complete-case analysis.",
    standardize_can_fix = analysis_grid_ok && n_complete < n_subjects,
    requires_manual_resolution = !analysis_grid_ok
  )

  treatment_conversion <- .pd_binary_conversion(data[[A_col]])
  treatment_missing <- which(is.na(data[[A_col]]))
  treatment_invalid_rows <- treatment_conversion$invalid_rows
  treatment_invalid_subjects <- unique(
    .pd_key(data[[id_col]][treatment_invalid_rows])
  )
  diagnostics$treatment_invalid_rows <- treatment_invalid_rows
  diagnostics$treatment_invalid_subjects <- treatment_invalid_subjects
  add_check(
    "treatment_encoding",
    treatment_conversion$ok && treatment_conversion$canonical &&
      !length(treatment_missing),
    if (treatment_conversion$ok && !length(treatment_missing)) "warning" else "error",
    paste(
      "Class:", treatment_conversion$original_class,
      "; values:", .pd_display_values(data[[A_col]]),
      "; missing rows:", length(treatment_missing),
      "; invalid rows:", length(treatment_invalid_rows),
      "; affected subjects:", length(treatment_invalid_subjects),
      if (length(treatment_conversion$invalid)) {
        paste0("; invalid values: ", paste(treatment_conversion$invalid, collapse = ", "))
      } else {
        ""
      },
      "."
    ),
    if (treatment_conversion$ok) {
      "Standardization safely converts explicit FALSE/TRUE or \"0\"/\"1\" encodings to integer 0/1; missing treatment requires `drop = TRUE` or manual recovery."
    } else {
      "Recode ambiguous treatment values manually."
    },
    standardize_can_fix = treatment_conversion$ok &&
      (!treatment_conversion$canonical || length(treatment_missing) > 0L),
    requires_manual_resolution = !treatment_conversion$ok,
    analysis_blocking = !(
      treatment_conversion$ok && treatment_conversion$canonical &&
        !length(treatment_missing)
    )
  )

  survival_conversion <- .pd_binary_conversion(data[[S_col]])
  survival_missing <- which(is.na(data[[S_col]]))
  survival_invalid_rows <- survival_conversion$invalid_rows
  diagnostics$survival_invalid_rows <- survival_invalid_rows
  add_check(
    "survival_encoding",
    survival_conversion$ok && survival_conversion$canonical &&
      !length(survival_missing),
    if (survival_conversion$ok && !length(survival_missing)) "warning" else "error",
    paste(
      "Class:", survival_conversion$original_class,
      "; values:", .pd_display_values(data[[S_col]]),
      "; missing rows:", length(survival_missing),
      "; invalid rows:", length(survival_invalid_rows),
      if (length(survival_conversion$invalid)) {
        paste0("; invalid values: ", paste(survival_conversion$invalid, collapse = ", "))
      } else {
        ""
      },
      "."
    ),
    if (survival_conversion$ok) {
      "Standardization safely converts explicit binary encodings; missing status requires `drop = TRUE` or manual recovery."
    } else {
      "Recode ambiguous survival values manually."
    },
    standardize_can_fix = survival_conversion$ok &&
      (!survival_conversion$canonical || length(survival_missing) > 0L),
    requires_manual_resolution = !survival_conversion$ok,
    analysis_blocking = !(
      survival_conversion$ok && survival_conversion$canonical &&
        !length(survival_missing)
    )
  )

  A_numeric <- if (treatment_conversion$ok) treatment_conversion$value else
    rep(NA_integer_, nrow(data))
  S_numeric <- if (survival_conversion$ok) survival_conversion$value else
    rep(NA_integer_, nrow(data))

  treatment_changes <- character()
  if (length(id_groups) && treatment_conversion$ok) {
    treatment_changes <- names(id_groups)[vapply(id_groups, function(idx) {
      length(unique(stats::na.omit(A_numeric[idx]))) > 1L
    }, logical(1))]
  }
  diagnostics$treatment_changes <- treatment_changes
  add_check(
    "treatment_consistency_within_subject",
    !length(treatment_changes),
    "error",
    paste(length(treatment_changes), "subjects change treatment over follow-up.",
          if (length(treatment_changes)) {
            paste("Affected IDs:", paste(treatment_changes, collapse = ", "), ".")
          } else ""),
    "Verify baseline treatment coding or use a method designed for time-varying treatment.",
    requires_manual_resolution = length(treatment_changes) > 0L
  )

  impossible_survival <- character()
  if (length(id_groups) && survival_conversion$ok && time_conversion$ok) {
    impossible_survival <- names(id_groups)[vapply(id_groups, function(idx) {
      ord <- idx[order(numeric_time[idx], na.last = TRUE)]
      s <- stats::na.omit(S_numeric[ord])
      length(s) > 1L && any(diff(s) > 0)
    }, logical(1))]
  }
  diagnostics$impossible_survival_transitions <- impossible_survival
  add_check(
    "survival_consistency_within_subject",
    !length(impossible_survival),
    "error",
    paste(length(impossible_survival), "subjects transition from S = 0 back to S = 1.",
          if (length(impossible_survival)) {
            paste("Affected IDs:", paste(impossible_survival, collapse = ", "), ".")
          } else ""),
    "Correct the survival history manually; automatic repair is unsafe.",
    requires_manual_resolution = length(impossible_survival) > 0L
  )

  if (identical(outcome_type, "B")) {
    outcome_conversion <- .pd_binary_conversion(data[[Y_col]])
    outcome_invalid_rows <- outcome_conversion$invalid_rows
    outcome_ok <- outcome_conversion$ok
    outcome_canonical <- outcome_conversion$canonical
    Y_numeric <- if (outcome_ok) outcome_conversion$value else
      rep(NA_integer_, nrow(data))
    outcome_detail <- paste(
      "Binary outcome class:", outcome_conversion$original_class,
      "; values:", .pd_display_values(data[[Y_col]]),
      "; invalid rows:", length(outcome_invalid_rows),
      if (length(outcome_conversion$invalid)) {
        paste0("; invalid values: ", paste(outcome_conversion$invalid, collapse = ", "))
      } else {
        ""
      },
      "."
    )
  } else {
    outcome_conversion <- .pd_numeric_conversion(data[[Y_col]])
    outcome_ok <- outcome_conversion$ok
    outcome_canonical <- outcome_conversion$canonical
    Y_numeric <- if (outcome_ok) outcome_conversion$value else
      rep(NA_real_, nrow(data))
    outcome_detail <- paste(
      "Continuous outcome class:", outcome_conversion$original_class,
      if (length(outcome_conversion$invalid_rows)) {
        paste0("; invalid/non-finite rows: ",
               paste(outcome_conversion$invalid_rows, collapse = ", "))
      } else {
        "; all nonmissing values are finite numeric values"
      },
      "."
    )
  }
  diagnostics$outcome_invalid_rows <- outcome_conversion$invalid_rows %||%
    if (identical(outcome_type, "B")) outcome_invalid_rows else integer()
  add_check(
    "outcome_type_and_encoding",
    outcome_ok && outcome_canonical,
    if (outcome_ok) "warning" else "error",
    outcome_detail,
    if (outcome_ok && !outcome_canonical) {
      "Standardization will perform the unambiguous numeric conversion."
    } else if (outcome_ok) {
      "No outcome recoding is required."
    } else {
      "Resolve invalid outcome values manually."
    },
    standardize_can_fix = outcome_ok && !outcome_canonical,
    requires_manual_resolution = !outcome_ok,
    analysis_blocking = !(outcome_ok && outcome_canonical)
  )

  structural_missing <- which(S_numeric == 0 & is.na(Y_numeric))
  add_check(
    "structural_outcome_missingness_after_death",
    TRUE,
    "information",
    paste(
      length(structural_missing), "records (",
      round(100 * length(structural_missing) / nrow(data), 3),
      "%) have S = 0 and Y = NA; this is expected structural missingness."
    ),
    "Do not impute these outcomes or replace them with observed zeros.",
    analysis_blocking = FALSE
  )

  observed_after_death <- which(S_numeric == 0 & !is.na(Y_numeric))
  diagnostics$outcome_observed_after_death_rows <- observed_after_death
  add_check(
    "outcome_observed_after_death",
    !length(observed_after_death),
    "error",
    paste(length(observed_after_death), "records have S = 0 and an observed outcome.",
          if (length(observed_after_death)) {
            paste("Original rows:", paste(observed_after_death, collapse = ", "), ".")
          } else ""),
    "Verify and remove or recode these outcomes manually; the package will not silently alter observed values.",
    requires_manual_resolution = length(observed_after_death) > 0L
  )

  missing_alive <- which(S_numeric == 1 & is.na(Y_numeric))
  missing_alive_by_time <- if (length(analysis_times)) {
    data.frame(
      time = analysis_times,
      missing_records = vapply(analysis_times, function(t) {
        sum(numeric_time[missing_alive] == t, na.rm = TRUE)
      }, integer(1)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(time = numeric(), missing_records = integer())
  }
  missing_alive_subjects <- unique(.pd_key(data[[id_col]][missing_alive]))
  diagnostics$outcome_missing_alive_rows <- missing_alive
  diagnostics$outcome_missing_alive_by_time <- missing_alive_by_time
  add_check(
    "outcome_missingness_among_survivors",
    !length(missing_alive),
    "error",
    paste(
      length(missing_alive), "records across", length(missing_alive_subjects),
      "subjects have S = 1 and Y = NA. Counts by time:",
      if (nrow(missing_alive_by_time)) {
        paste(
          paste0(.pd_time_label(missing_alive_by_time$time), "=",
                 missing_alive_by_time$missing_records),
          collapse = ", "
        )
      } else {
        "<unavailable>"
      },
      "."
    ),
    "Handle survivor outcome missingness using a study-appropriate method, or use `drop = TRUE` for an explicitly reported subject-level deletion.",
    standardize_can_fix = length(missing_alive) > 0L
  )

  covariate_missing <- data.frame(
    covariate = covariates,
    missing_records = integer(length(covariates)),
    affected_subjects = integer(length(covariates)),
    affected_subject_percent = numeric(length(covariates)),
    stringsAsFactors = FALSE
  )
  if (length(covariates)) {
    for (i in seq_along(covariates)) {
      variable <- covariates[i]
      rows <- which(is.na(data[[variable]]))
      affected <- unique(.pd_key(data[[id_col]][rows]))
      covariate_missing$missing_records[i] <- length(rows)
      covariate_missing$affected_subjects[i] <- length(affected)
      covariate_missing$affected_subject_percent[i] <-
        if (n_subjects) 100 * length(affected) / n_subjects else 0
    }
  }
  diagnostics$covariate_missing <- covariate_missing
  covariate_missing_total <- sum(covariate_missing$missing_records)
  add_check(
    "missing_covariates",
    covariate_missing_total == 0L,
    "error",
    if (length(covariates)) {
      paste(
        paste0(
          covariate_missing$covariate, "=",
          covariate_missing$missing_records, " records/",
          covariate_missing$affected_subjects, " subjects (",
          round(covariate_missing$affected_subject_percent, 3), "%)"
        ),
        collapse = "; "
      )
    } else {
      "No covariates were mapped for data-level completeness checks."
    },
    "Impute or otherwise handle missing covariates externally, or use `drop = TRUE` for reported complete-case deletion.",
    standardize_can_fix = covariate_missing_total > 0L
  )

  ordering_complete <- which(!is.na(data[[id_col]]) & !is.na(numeric_time))
  expected_order <- ordering_complete[
    order(data[[id_col]][ordering_complete],
          numeric_time[ordering_complete])
  ]
  ordered_correctly <- identical(ordering_complete, expected_order)
  standardized_grid <- if (length(analysis_times)) {
    as.numeric(seq.int(0L, length(analysis_times) - 1L))
  } else {
    numeric()
  }
  observed_window <- observed_times[
    observed_times >= baseline_time & observed_times <= cutoff_time
  ]
  add_check(
    "time_coding_and_order",
    time_conversion$ok && ordered_correctly &&
      isTRUE(all.equal(observed_window, standardized_grid)),
    "warning",
    paste(
      "Raw times:", paste(.pd_time_label(observed_times), collapse = ", "),
      "; rows are", if (ordered_correctly) "ordered" else "not ordered",
      "by ID and time."
    ),
    "Standardization sorts records and maps the analysis grid to integers 0, 1, ..., n.",
    standardize_can_fix = TRUE,
    analysis_blocking = !(
      time_conversion$ok && ordered_correctly &&
        isTRUE(all.equal(observed_window, standardized_grid))
    )
  )

  id_canonical <- .pd_is_consecutive_integer_id(data[[id_col]])
  add_check(
    "id_coding",
    id_canonical && ordered_correctly,
    "warning",
    paste(
      "ID class:", paste(class(data[[id_col]]), collapse = "/"),
      ";", length(id_values), "unique nonmissing subjects;",
      if (id_canonical) "consecutive integer coding." else "noncanonical coding."
    ),
    "Standardization preserves an ID audit map and assigns consecutive integer IDs.",
    standardize_can_fix = TRUE,
    analysis_blocking = !(id_canonical && ordered_correctly)
  )

  baseline_rows <- which(numeric_time == baseline_time & !is.na(data[[id_col]]))
  baseline_A <- A_numeric[baseline_rows]
  baseline_ids <- .pd_key(data[[id_col]][baseline_rows])
  unique_baseline <- !duplicated(baseline_ids)
  group_counts <- table(factor(
    baseline_A[unique_baseline], levels = c(0, 1)
  ))
  both_groups <- all(as.integer(group_counts) > 0L)
  diagnostics$treatment_group_counts <- data.frame(
    treatment = c(0L, 1L),
    baseline_subjects = as.integer(group_counts),
    stringsAsFactors = FALSE
  )
  add_check(
    "treatment_group_availability",
    both_groups,
    "error",
    paste(
      "Baseline subjects: treatment 0 =", group_counts[1L],
      "; treatment 1 =", group_counts[2L], "."
    ),
    "Revise the analysis population or mapping; both groups are required.",
    requires_manual_resolution = !both_groups
  )

  near_zero <- covariates[vapply(covariates, function(variable) {
    .pd_near_zero_variation(data[[variable]])
  }, logical(1))]
  diagnostics$near_zero_variation_covariates <- near_zero
  add_check(
    "covariate_variation",
    !length(near_zero),
    "warning",
    if (length(near_zero)) {
      paste("Constant or near-zero-variation covariates:",
            paste(near_zero, collapse = ", "), ".")
    } else if (length(covariates)) {
      "No mapped covariate has constant or near-zero variation."
    } else {
      "No mapped covariates were supplied."
    },
    "Remove or revise problematic covariates before model fitting.",
    analysis_blocking = FALSE
  )

  add_check(
    "retained_sample_after_optional_dropping",
    TRUE,
    "information",
    paste(
      n_subjects,
      "subjects are present before optional standardization-time deletion."
    ),
    "If `drop = TRUE` is used, review the attached attrition report before interpretation.",
    analysis_blocking = FALSE
  )

  checks_df <- do.call(rbind, checks)
  failed <- !checks_df$passed
  valid <- !any(failed & checks_df$severity == "error")
  manual_resolution_required <- any(
    failed & checks_df$requires_manual_resolution
  )
  ready_for_analysis <- !any(failed & checks_df$analysis_blocking)
  can_standardize <- !manual_resolution_required
  report <- structure(
    list(
      valid = valid,
      ready_for_analysis = ready_for_analysis,
      manual_resolution_required = manual_resolution_required,
      can_standardize = can_standardize,
      checks = checks_df,
      settings = list(mapping = mapping),
      diagnostics = diagnostics
    ),
    class = "pd_data_check"
  )
  if (strict && !ready_for_analysis) {
    blockers <- checks_df$check[failed & checks_df$analysis_blocking]
    .pd_stop("Data validation failed: ", paste(blockers, collapse = ", "), ".")
  }
  report
}

.pd_standardize_data_impl <- function(data, mapping, drop = FALSE) {
  if (length(drop) != 1L || is.na(drop) || !is.logical(drop)) {
    .pd_stop("`drop` must be TRUE or FALSE.")
  }
  initial <- .pd_check_data_impl(
    data = data, mapping = mapping, strict = FALSE
  )
  mapping <- initial$settings$mapping
  data <- .pd_as_data_frame(data)

  manual <- initial$checks$check[
    !initial$checks$passed & initial$checks$requires_manual_resolution
  ]
  if (length(manual)) {
    .pd_stop(
      "Standardization stopped because manual resolution is required for: ",
      paste(manual, collapse = ", "), "."
    )
  }

  baseline_time <- mapping$baseline_time
  cutoff_time <- mapping$cutoff_time
  covariates <- mapping$covariates
  outcome_type <- mapping$y_type
  id_col <- mapping$id_col
  time_col <- mapping$time_col
  A_col <- mapping$A_col
  S_col <- mapping$S_col
  Y_col <- mapping$Y_col

  time_conversion <- .pd_numeric_conversion(data[[time_col]])
  treatment_conversion <- .pd_binary_conversion(data[[A_col]])
  survival_conversion <- .pd_binary_conversion(data[[S_col]])
  outcome_conversion <- if (identical(outcome_type, "B")) {
    .pd_binary_conversion(data[[Y_col]])
  } else {
    .pd_numeric_conversion(data[[Y_col]])
  }
  if (!time_conversion$ok || !treatment_conversion$ok ||
      !survival_conversion$ok || !outcome_conversion$ok) {
    .pd_stop("Standardization encountered a non-convertible mapped variable.")
  }
  data[[time_col]] <- time_conversion$value
  data[[A_col]] <- treatment_conversion$value
  data[[S_col]] <- survival_conversion$value
  data[[Y_col]] <- outcome_conversion$value

  original_rows <- nrow(data)
  original_ids <- unique(.pd_key(data[[id_col]][!is.na(data[[id_col]])]))
  drop_reasons <- data.frame(
    subject = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )
  unidentified_rows <- which(is.na(data[[id_col]]) | is.na(data[[time_col]]))
  if (length(unidentified_rows)) {
    if (!drop) {
      .pd_stop(
        "Rows with missing ID/time require manual recovery or `drop = TRUE`. ",
        "See `DataCheck()` for original row numbers."
      )
    }
    data <- data[-unidentified_rows, , drop = FALSE]
  }

  # Every actual observed visit within the mapped baseline-to-cutoff window
  # belongs to the analysis grid. Only rows outside that window are removed.
  in_analysis_grid <- data[[time_col]] >= baseline_time &
    data[[time_col]] <= cutoff_time
  rows_outside_window <- sum(!in_analysis_grid)
  data <- data[in_analysis_grid, , drop = FALSE]
  required_times <- sort(unique(stats::na.omit(data[[time_col]])))
  if (!length(required_times) || !baseline_time %in% required_times ||
      !cutoff_time %in% required_times) {
    .pd_stop("The data must contain baseline and cutoff observations after time conversion.")
  }
  id_key <- .pd_key(data[[id_col]])
  id_groups <- split(seq_len(nrow(data)), id_key)
  mark_subjects <- function(subjects, reason) {
    if (!length(subjects)) return(invisible(NULL))
    drop_reasons <<- rbind(
      drop_reasons,
      data.frame(subject = subjects, reason = reason,
                 stringsAsFactors = FALSE)
    )
    invisible(NULL)
  }

  incomplete <- names(id_groups)[vapply(id_groups, function(idx) {
    !all(required_times %in% data[[time_col]][idx])
  }, logical(1))]
  missing_model_data <- names(id_groups)[vapply(id_groups, function(idx) {
    anyNA(data[[A_col]][idx]) || anyNA(data[[S_col]][idx]) ||
      any(data[[S_col]][idx] == 1 & is.na(data[[Y_col]][idx])) ||
      (length(covariates) &&
         any(!stats::complete.cases(data[idx, covariates, drop = FALSE])))
  }, logical(1))]
  mark_subjects(incomplete, "missing_analysis_visit")
  mark_subjects(missing_model_data, "missing_required_analysis_value")
  subjects_to_drop <- unique(c(incomplete, missing_model_data))
  if (length(subjects_to_drop) && !drop) {
    .pd_stop(
      length(subjects_to_drop),
      " subjects require deletion to create analysis-ready data. ",
      "Resolve the data manually or rerun with `drop = TRUE` and review the attrition report."
    )
  }
  if (length(subjects_to_drop)) {
    data <- data[!.pd_key(data[[id_col]]) %in% subjects_to_drop, , drop = FALSE]
  }
  if (!nrow(data)) {
    .pd_stop("No observations remain after standardization.")
  }

  time_map <- data.frame(
    raw_time = as.numeric(required_times),
    standardized_time = seq_along(required_times) - 1L,
    stringsAsFactors = FALSE
  )
  mapped_time <- time_map$standardized_time[
    match(data[[time_col]], time_map$raw_time)
  ]
  if (anyNA(mapped_time)) {
    .pd_stop("Observed analysis times do not match the baseline-to-cutoff grid.")
  }
  data[[time_col]] <- as.integer(mapped_time)

  raw_id <- .pd_key(data[[id_col]])
  id_levels <- unique(raw_id[order(data[[id_col]])])
  id_map <- data.frame(
    raw_id = id_levels,
    standardized_id = seq_along(id_levels),
    stringsAsFactors = FALSE
  )
  data[[id_col]] <- as.integer(id_map$standardized_id[
    match(raw_id, id_map$raw_id)
  ])
  data[[A_col]] <- as.integer(data[[A_col]])
  data[[S_col]] <- as.integer(data[[S_col]])
  if (identical(outcome_type, "B")) {
    data[[Y_col]] <- as.integer(data[[Y_col]])
  } else {
    data[[Y_col]] <- as.numeric(data[[Y_col]])
  }

  standardized_mapping <- Mapping(
    id = id_col, time = time_col, treatment = A_col,
    survival = S_col, outcome = Y_col,
    baseline_time = 0L,
    cutoff_time = nrow(time_map) - 1L,
    covariates = covariates,
    interest_vars = mapping$interest_vars,
    y_type = outcome_type
  )
  data <- data[order(data[[id_col]], data[[time_col]]), , drop = FALSE]
  rownames(data) <- NULL
  attr(data, "pd_mapping") <- standardized_mapping
  class(data) <- unique(c("pd_data", class(data)))
  final <- .pd_check_data_impl(data, mapping = standardized_mapping, strict = FALSE)

  removed_subjects <- unique(drop_reasons$subject)
  retained_subjects <- length(unique(data[[id_col]]))
  attrition <- list(
    original_rows = original_rows,
    rows_outside_analysis_window = rows_outside_window,
    unidentified_rows_removed = length(unidentified_rows),
    original_subjects = length(original_ids),
    removed_subjects = removed_subjects,
    removed_subjects_by_reason = unique(drop_reasons),
    retained_subjects = retained_subjects,
    retained_percent = if (length(original_ids)) {
      100 * retained_subjects / length(original_ids)
    } else {
      0
    }
  )
  attrition_row <- final$checks$check ==
    "retained_sample_after_optional_dropping"
  final$checks$details[attrition_row] <- paste(
    "Original subjects:", attrition$original_subjects,
    "; removed subjects:", length(attrition$removed_subjects),
    "; retained subjects:", attrition$retained_subjects,
    paste0("(", round(attrition$retained_percent, 3), "%).")
  )
  final$attrition <- attrition
  final$ready_for_analysis <- !any(
    !final$checks$passed & final$checks$analysis_blocking
  )
  attr(data, "pd_mapping") <- standardized_mapping
  attr(data, "pd_original_mapping") <- mapping
  attr(data, "pd_check") <- final
  attr(data, "pd_standardization") <- list(
    time_map = time_map,
    id_map = id_map,
    attrition = attrition,
    initial_check = initial
  )
  if (!isTRUE(final$ready_for_analysis)) {
    blockers <- final$checks$check[
      !final$checks$passed & final$checks$analysis_blocking
    ]
    .pd_warn(
      "Standardization completed, but the data are not ready for analysis: ",
      paste(blockers, collapse = ", "), "."
    )
  }
  data
}

#' Round display-only diagnostics at the public return boundary
#'
#' Row numbers, counts, identifiers, logical readiness flags, and raw time
#' values retain their original storage modes and values.
#'
#' @noRd
.pd_round_data_check <- function(x) {
  if (!inherits(x, "pd_data_check")) {
    return(x)
  }
  covariate_missing <- x$diagnostics$covariate_missing
  if (is.data.frame(covariate_missing) &&
      "affected_subject_percent" %in% names(covariate_missing)) {
    covariate_missing$affected_subject_percent <- .pd_round_output(
      covariate_missing$affected_subject_percent
    )
    x$diagnostics$covariate_missing <- covariate_missing
  }
  if (is.list(x$attrition)) {
    x$attrition$retained_percent <- .pd_round_output(
      x$attrition$retained_percent
    )
  }
  x
}

#' @noRd
#' @export
print.pd_data_check <- function(x, ...) {
  cat("PDRobust data validation\n")
  cat("  Manual resolution required:",
      if (isTRUE(x$manual_resolution_required)) "YES" else "NO", "\n")
  cat("  Ready for analysis:",
      if (isTRUE(x$ready_for_analysis)) "YES" else "NO", "\n")
  print(x$checks, row.names = FALSE)
  invisible(x)
}

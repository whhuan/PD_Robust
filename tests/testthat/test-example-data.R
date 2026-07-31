test_that("built-in example data follow their documented raw contracts", {
  utils::data("BiSample", package = "PDRobust", envir = environment())
  utils::data(
    "ImperfectConSample", package = "PDRobust", envir = environment()
  )

  expect_s3_class(BiSample, "data.frame")
  expect_true(all(c("id", "time", "A", "S", "Y") %in% names(BiSample)))
  expect_gt(nrow(BiSample), 0L)

  imperfect_columns <- c(
    "patient_id", "visit_month", "treatment", "alive_status",
    "clinical_outcome", paste0("X", 1:6)
  )
  expect_s3_class(ImperfectConSample, "data.frame")
  expect_true(all(imperfect_columns %in% names(ImperfectConSample)))
  expect_false(any(c("id", "time", "A", "S", "Y") %in%
                     names(ImperfectConSample)))
  expect_type(ImperfectConSample$patient_id, "character")
  expect_type(ImperfectConSample$visit_month, "character")
  expect_type(ImperfectConSample$treatment, "character")
  expect_type(ImperfectConSample$alive_status, "character")
  expect_setequal(unique(ImperfectConSample$visit_month), c("0", "6", "12"))
  expect_true(all(
    grepl("^PT-[0-9]{4}$", stats::na.omit(ImperfectConSample$patient_id))
  ))

  expect_gte(sum(is.na(ImperfectConSample$patient_id)), 1L)
  expect_gte(sum(is.na(ImperfectConSample$X1)), 1L)
  expect_gte(sum(
    ImperfectConSample$alive_status == "1" &
      is.na(ImperfectConSample$clinical_outcome),
    na.rm = TRUE
  ), 1L)
  expect_true(all(is.na(
    ImperfectConSample$clinical_outcome[
      ImperfectConSample$alive_status == "0"
    ]
  )))
  canonical_order <- order(
    ImperfectConSample$patient_id,
    as.numeric(ImperfectConSample$visit_month),
    na.last = TRUE
  )
  expect_false(identical(canonical_order, seq_len(nrow(ImperfectConSample))))
})

test_that("built-in example data enter the mapping-driven public workflow", {
  utils::data("BiSample", package = "PDRobust", envir = environment())
  utils::data(
    "ImperfectConSample", package = "PDRobust", envir = environment()
  )

  map_b <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = min(BiSample$time),
    cutoff_time = max(BiSample$time),
    covariates = c("X1", "X2", "X4"),
    interest_vars = c("X1", "X2"),
    y_type = "B"
  )
  expect_s3_class(DataCheck(BiSample, map_b), "pd_data_check")
  expect_s3_class(DataStandard(BiSample, map_b), "pd_data")

  raw_time <- as.numeric(ImperfectConSample$visit_month)
  expect_true(all(is.finite(raw_time)))
  map_c <- Mapping(
    id = "patient_id",
    time = "visit_month",
    treatment = "treatment",
    survival = "alive_status",
    outcome = "clinical_outcome",
    baseline_time = 0,
    cutoff_time = 12,
    covariates = paste0("X", 1:6),
    interest_vars = c("X1", "X2"),
    y_type = "C"
  )
  check <- DataCheck(ImperfectConSample, map_c)
  expect_s3_class(check, "pd_data_check")
  expect_true(check$can_standardize)
  expect_false(check$ready_for_analysis)
  expected_recoverable_checks <- c(
    "missing_id_or_time",
    "complete_longitudinal_structure",
    "outcome_missingness_among_survivors",
    "missing_covariates",
    "time_coding_and_order",
    "id_coding"
  )
  recoverable <- check$checks[
    check$checks$check %in% expected_recoverable_checks, , drop = FALSE
  ]
  expect_setequal(recoverable$check, expected_recoverable_checks)
  expect_true(all(recoverable$standardize_can_fix))

  prepared <- DataStandard(ImperfectConSample, map_c, drop = TRUE)
  prepared_mapping <- attr(prepared, "pd_mapping")
  attrition <- attr(prepared, "pd_standardization")$attrition
  expect_s3_class(prepared, "pd_data")
  expect_true(attr(prepared, "pd_check")$ready_for_analysis)
  expect_identical(sort(unique(prepared$visit_month)), 0:2)
  expect_true(is.integer(prepared$patient_id))
  expect_true(is.integer(prepared$treatment))
  expect_true(is.integer(prepared$alive_status))
  expect_identical(prepared_mapping$baseline_time, 0)
  expect_identical(prepared_mapping$cutoff_time, 2)
  expect_gte(attrition$unidentified_rows_removed, 1L)
  expect_gte(length(attrition$removed_subjects), 4L)

  ps <- suppressWarnings(PSPred(
    treatment ~ X1 + X2 + X4,
    prepared, prepared, prepared_mapping
  ))
  p0 <- suppressWarnings(PrinPred(
    alive_status ~ X1 + X2 + X4 + treatment + visit_month,
    prepared, prepared, treatment = 0, mapping = prepared_mapping
  ))
  outcome_fit <- prepared[prepared$alive_status == 1L, , drop = FALSE]
  mu1 <- suppressWarnings(OutPred(
    clinical_outcome ~ X1 + X2 + treatment,
    outcome_fit, prepared, a = 1, mapping = prepared_mapping
  ))
  expect_true(all(is.finite(c(ps, p0, mu1))))
})

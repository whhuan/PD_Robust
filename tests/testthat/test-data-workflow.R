test_that("mapping validates endpoints without storing target times", {
  mapping <- Mapping(
    id = "subject", time = "visit", treatment = "treatment",
    survival = "alive", outcome = "response",
    baseline_time = 3, cutoff_time = 9,
    covariates = c("x1", "x2"), interest_vars = "x1",
    y_type = "B"
  )
  expect_s3_class(mapping, "pd_mapping")
  expect_identical(mapping$baseline_time, 3)
  expect_identical(mapping$cutoff_time, 9)
  expect_false("target_time" %in% names(mapping))
  expect_error(
    Mapping(
      id = "id", time = "time", treatment = "A",
      survival = "S", outcome = "Y",
      baseline_time = 3, cutoff_time = 9,
      covariates = "x1", interest_vars = "x2", y_type = "C"
    ),
    "must also be listed"
  )
})

test_that("checking reports duplicates and preserves the complete time grid", {
  workflow <- make_pd_workflow(times = 0:3)
  check <- DataCheck(workflow$raw, workflow$mapping)
  expect_s3_class(check, "pd_data_check")
  expect_true(all(c(
    "analysis_time_grid", "complete_longitudinal_structure",
    "duplicate_id_time_records", "treatment_encoding",
    "survival_consistency_within_subject"
  ) %in% check$checks$check))
  expect_equal(check$diagnostics$missing_by_time$time, 0:3)

  duplicated <- rbind(workflow$raw, workflow$raw[1, , drop = FALSE])
  duplicate_check <- DataCheck(duplicated, workflow$mapping)
  expect_false(duplicate_check$ready_for_analysis)
  expect_true(duplicate_check$manual_resolution_required)
  expect_gte(length(duplicate_check$diagnostics$duplicate_rows), 2L)
})

test_that("standardization converts explicit binary encodings and retains visits", {
  raw <- make_pd_raw(times = c(3, 6, 8, 9))
  raw$id <- paste0("S", raw$id)
  raw$A <- factor(as.character(raw$A), levels = c("0", "1"))
  raw$S <- factor(as.character(raw$S), levels = c("0", "1"))
  mapping <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 3, cutoff_time = 9,
    covariates = c("X1", "X2", "X4"),
    interest_vars = c("X1", "X2"), y_type = "C"
  )

  prepared <- DataStandard(raw, mapping)
  standardized_mapping <- attr(prepared, "pd_mapping")
  expect_s3_class(prepared, "pd_data")
  expect_true(is.integer(prepared$A))
  expect_true(is.integer(prepared$S))
  expect_identical(sort(unique(prepared$time)), 0:3)
  expect_equal(standardized_mapping$baseline_time, 0)
  expect_equal(standardized_mapping$cutoff_time, 3)
  expect_false("target_time" %in% names(standardized_mapping))
  expect_equal(
    attr(prepared, "pd_standardization")$time_map$raw_time,
    c(3, 6, 8, 9)
  )
})

test_that("drop is explicit and produces a subject-level attrition report", {
  raw <- make_pd_raw(times = 0:2)
  original_subjects <- length(unique(raw$id))
  raw <- raw[!(raw$id == 1 & raw$time == 1), , drop = FALSE]
  mapping <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0, cutoff_time = 2,
    covariates = c("X1", "X2", "X4"),
    interest_vars = c("X1", "X2"), y_type = "C"
  )
  expect_error(DataStandard(raw, mapping), "drop = TRUE")
  prepared <- DataStandard(raw, mapping, drop = TRUE)
  attrition <- attr(prepared, "pd_standardization")$attrition
  expect_true("1" %in% attrition$removed_subjects)
  expect_equal(attrition$retained_subjects, original_subjects - 1L)
  expect_true(attr(prepared, "pd_check")$ready_for_analysis)
})

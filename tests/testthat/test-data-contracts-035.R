test_that("simulation helper is deterministic and has no RNG side effects", {
  set.seed(101)
  before <- .Random.seed
  continuous_1 <- simulate_pd_test_data(outcome = "continuous", seed = 7)
  after <- .Random.seed
  continuous_2 <- simulate_pd_test_data(outcome = "continuous", seed = 7)
  binary <- simulate_pd_test_data(outcome = "binary", seed = 7)

  expect_identical(after, before)
  expect_identical(continuous_1, continuous_2)
  expect_true(is.numeric(continuous_1$Y))
  expect_true(all(stats::na.omit(binary$Y) %in% c(0, 1)))
})

test_that("simulation helper creates each documented invalid dataset", {
  invalid_kinds <- c(
    "missing_column", "invalid_binary", "duplicate", "missing_visit",
    "survivor_missing", "resurrection", "zero_variance",
    "rank_deficient", "separation", "nonvarying_outcome"
  )
  generated <- lapply(
    invalid_kinds,
    function(kind) simulate_pd_test_data(n = 60, invalid = kind)
  )
  names(generated) <- invalid_kinds

  expect_false("X2" %in% names(generated$missing_column))
  expect_true(any(generated$invalid_binary$A == 2))
  expect_true(anyDuplicated(generated$duplicate[c("id", "time")]) > 0L)
  expect_lt(nrow(generated$missing_visit), 60L * 3L)
  expect_true(any(generated$survivor_missing$S == 1 &
                    is.na(generated$survivor_missing$Y)))
  expect_true(any(diff(generated$resurrection$S[
    generated$resurrection$id == 1
  ]) > 0))
  expect_equal(length(unique(generated$zero_variance$X1)), 1L)
  expect_identical(generated$rank_deficient$X1,
                   generated$rank_deficient$X2)
  expect_identical(generated$separation$A,
                   as.integer(generated$separation$X1 > 0))
})

test_that("DataCheck validates its public contract without changing input", {
  raw <- simulate_pd_test_data(n = 80)
  map <- make_pd_test_mapping(raw)
  original <- raw

  first <- DataCheck(raw, map)
  second <- DataCheck(raw, map)
  expect_s3_class(first, "pd_data_check")
  expect_identical(first, second)
  expect_identical(raw, original)
  expect_true(first$ready_for_analysis)
  expect_named(first, c(
    "valid", "ready_for_analysis", "manual_resolution_required",
    "can_standardize", "checks", "settings", "diagnostics"
  ))
  expect_error(DataCheck(raw, map, strict = 1), "`strict`")
  expect_error(
    DataCheck(raw, list(), strict = FALSE),
    "`mapping` must be returned by `Mapping\\(\\)`"
  )
})

test_that("DataCheck distinguishes supported from unsupported encodings", {
  raw <- simulate_pd_test_data(n = 80, outcome = "binary")
  map <- make_pd_test_mapping(raw, "binary")
  supported <- raw
  supported$A <- factor(as.character(supported$A), levels = c("0", "1"))
  supported$S <- as.character(supported$S)
  supported$Y <- ifelse(
    is.na(supported$Y), NA_character_, as.character(supported$Y)
  )
  check <- DataCheck(supported, map)
  expect_true(check$can_standardize)
  expect_false(check$ready_for_analysis)

  unsupported <- raw
  unsupported$A <- ifelse(unsupported$A == 1, "treated", "control")
  bad <- DataCheck(unsupported, map)
  expect_false(bad$can_standardize)
  expect_true(bad$manual_resolution_required)
  expect_error(DataStandard(unsupported, map), "manual resolution")
})

test_that("DataCheck reports structural edge cases with stable diagnostics", {
  valid <- simulate_pd_test_data(n = 80)
  map <- make_pd_test_mapping(valid)

  cases <- list(
    duplicate = simulate_pd_test_data(n = 80, invalid = "duplicate"),
    missing_visit = simulate_pd_test_data(n = 80, invalid = "missing_visit"),
    survivor_missing = simulate_pd_test_data(
      n = 80, invalid = "survivor_missing"
    ),
    resurrection = simulate_pd_test_data(n = 80, invalid = "resurrection")
  )
  checks <- lapply(cases, DataCheck, mapping = map)
  expect_false(checks$duplicate$ready_for_analysis)
  expect_true(checks$duplicate$manual_resolution_required)
  expect_false(checks$missing_visit$ready_for_analysis)
  expect_false(checks$survivor_missing$ready_for_analysis)
  expect_true(length(
    checks$survivor_missing$diagnostics$outcome_missing_alive_rows
  ) > 0L)
  expect_true(checks$resurrection$manual_resolution_required)

  empty <- valid[FALSE, , drop = FALSE]
  expect_false(DataCheck(empty, map)$ready_for_analysis)
  missing <- valid
  missing$X2 <- NULL
  expect_false(DataCheck(missing, map)$can_standardize)
})

test_that("DataStandard preserves inputs and attaches complete audit attributes", {
  raw <- simulate_pd_test_data(n = 80, times = c(3, 6, 9))
  raw$id <- paste0("patient-", raw$id)
  raw$A <- factor(as.character(raw$A), levels = c("0", "1"))
  raw$S <- as.character(raw$S)
  map <- make_pd_test_mapping(raw)
  original <- raw

  standardized <- DataStandard(raw, map)
  expect_identical(raw, original)
  expect_s3_class(standardized, "pd_data")
  expect_s3_class(attr(standardized, "pd_mapping"), "pd_mapping")
  expect_s3_class(attr(standardized, "pd_original_mapping"), "pd_mapping")
  expect_s3_class(attr(standardized, "pd_check"), "pd_data_check")
  expect_true(attr(standardized, "pd_check")$ready_for_analysis)
  expect_named(
    attr(standardized, "pd_standardization"),
    c("time_map", "id_map", "attrition", "initial_check")
  )
  expect_identical(sort(unique(standardized$time)), 0:2)
  expect_identical(sort(unique(standardized$id)), seq_len(80L))
  expect_true(is.integer(standardized$A))
  expect_true(is.integer(standardized$S))
})

test_that("DataStandard is value-idempotent and validates drop", {
  raw <- simulate_pd_test_data(n = 80)
  map <- make_pd_test_mapping(raw)
  first <- DataStandard(raw, map)
  second <- DataStandard(first, attr(first, "pd_mapping"))

  expect_identical(lapply(second, identity), lapply(first, identity))
  expect_identical(
    attr(second, "pd_mapping"),
    attr(first, "pd_mapping")
  )
  expect_error(DataStandard(raw, map, drop = 1), "`drop`")
  expect_error(DataStandard(raw, map, drop = NA), "`drop`")
})

test_that("DataStandard drop is reproducible and attrition is auditable", {
  raw <- simulate_pd_test_data(n = 80, invalid = "missing_visit")
  map <- make_pd_test_mapping(raw)
  expect_error(DataStandard(raw, map), "drop = TRUE")
  first <- DataStandard(raw, map, drop = TRUE)
  second <- DataStandard(raw, map, drop = TRUE)
  expect_identical(first, second)

  attrition <- attr(first, "pd_standardization")$attrition
  expect_true("1" %in% attrition$removed_subjects)
  expect_equal(attrition$original_subjects, 80L)
  expect_equal(attrition$retained_subjects, 79L)
  expect_equal(attrition$retained_percent, 98.75)
})

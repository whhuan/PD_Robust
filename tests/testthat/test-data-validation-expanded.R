test_that("DataCheck reports duplicate subject-time rows", {
  raw <- make_pd_raw(n = 30, times = 0:2)
  raw <- rbind(raw, raw[1, , drop = FALSE])
  map <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0, cutoff_time = 2,
    covariates = c("X1", "X2", "X4"), interest_vars = c("X1", "X2"),
    y_type = "C")
  check <- DataCheck(raw, map)
  expect_false(check$ready_for_analysis)
  duplicate_result <- check$checks[
    check$checks$check == "duplicate_id_time_records", , drop = FALSE
  ]
  expect_equal(nrow(duplicate_result), 1L)
  expect_false(duplicate_result$passed)
  expect_match(duplicate_result$details, "duplic", ignore.case = TRUE)
  expect_gte(length(check$diagnostics$duplicate_rows), 2L)
})

test_that("DataStandard safely converts explicit binary encodings", {
  raw <- make_pd_raw(n = 30, times = 0:2)
  raw$A <- factor(as.character(raw$A), levels = c("0", "1"))
  raw$S <- as.character(raw$S)
  map <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0, cutoff_time = 2,
    covariates = c("X1", "X2", "X4"), interest_vars = c("X1", "X2"),
    y_type = "C")
  dat <- DataStandard(raw, map)
  expect_true(is.numeric(dat$A) || is.integer(dat$A))
  expect_true(is.numeric(dat$S) || is.integer(dat$S))
  expect_true(all(dat$A %in% c(0, 1)))
  expect_true(all(dat$S %in% c(0, 1)))
})

test_that("DataStandard does not modify the raw input", {
  raw <- make_pd_raw(n = 30, times = 0:2)
  copy <- raw
  map <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0, cutoff_time = 2,
    covariates = c("X1", "X2", "X4"), interest_vars = c("X1", "X2"),
    y_type = "C")
  invisible(DataStandard(raw, map))
  expect_identical(raw, copy)
})

test_that("strict DataCheck stops on analysis-blocking defects", {
  raw <- make_pd_raw(n = 30, times = 0:2)
  raw <- raw[-1, , drop = FALSE]
  map <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0, cutoff_time = 2,
    covariates = c("X1", "X2", "X4"), interest_vars = c("X1", "X2"),
    y_type = "C")
  expect_error(DataCheck(raw, map, strict = TRUE))
})

test_that("target_time controls only HTESepT and may include baseline", {
  workflow <- make_pd_workflow(times = 0:2)
  sep <- HTESepT(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
    target_time = c(0, 2), B = 0, verbose = FALSE
  )
  pooled <- HTEAllT(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
    B = 0, verbose = FALSE
  )

  expect_equal(sort(unique(sep$summary$time)), c(0, 2))
  expect_equal(sep$target_time, c(0, 2))
  expect_equal(pooled$analysis_times, 0:2)
  expect_true(pooled$time_effect_estimable)
  expect_true("Time Effect" %in% pooled$summary$term)
})

test_that("single-time HTEAllT omits and documents the time effect", {
  workflow <- make_pd_workflow(times = 0)
  pooled <- HTEAllT(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
    B = 0, verbose = FALSE
  )
  expect_false(pooled$time_effect_estimable)
  expect_false("Time Effect" %in% pooled$summary$term)
  expect_match(pooled$note, "only one analysis time point", fixed = TRUE)
  expect_equal(pooled$analysis_times, 0)
})

test_that("standardization retains all actual visits inside the mapped window", {
  raw <- make_pd_raw(times = c(3, 6, 8, 9))
  map <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 3, cutoff_time = 9,
    covariates = c("X1", "X2", "X4"),
    interest_vars = c("X1", "X2"), y_type = "C"
  )
  prepared <- DataStandard(raw, map)
  expect_equal(sort(unique(prepared$time)), 0:3)
  time_map <- attr(prepared, "pd_standardization")$time_map
  expect_equal(time_map$raw_time, c(3, 6, 8, 9))
})

test_that("pooled analysis rejects prepared-data subsets that omit an intermediate visit", {
  workflow <- make_pd_workflow(times = 0:3)
  incomplete <- workflow$data[workflow$data$time != 2, , drop = FALSE]
  expect_error(
    HTEAllT(
      incomplete, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
      B = 0, verbose = FALSE
    ),
    "retain every standardized observed time"
  )
})

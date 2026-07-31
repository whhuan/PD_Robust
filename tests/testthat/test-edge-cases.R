test_that("single-time data support prediction and pooled HTE", {
  workflow <- make_pd_workflow(times = 0)
  map <- attr(workflow$data, "pd_mapping")
  expect_length(PSPred(workflow$ps_fo, workflow$data, workflow$data, map),
    nrow(workflow$data))
  expect_length(PrinPred(workflow$prin_fo, workflow$data, workflow$data, 0, map),
    nrow(workflow$data))
  fit <- HTEAllT(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, B = 0, verbose = FALSE)
  expect_false(fit$time_effect_estimable)
})

test_that("irregular raw times standardize to a complete integer grid", {
  raw <- make_pd_raw(times = c(2, 7, 11))
  map <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 2, cutoff_time = 11,
    covariates = c("X1", "X2", "X4"), interest_vars = c("X1", "X2"),
    y_type = "C")
  dat <- DataStandard(raw, map)
  expect_equal(sort(unique(dat$time)), 0:2)
  expect_equal(attr(dat, "pd_standardization")$time_map$raw_time, c(2, 7, 11))
})

test_that("binary outcome prediction follows logistic regression", {
  workflow <- make_pd_workflow(binary_outcome = TRUE)
  map <- attr(workflow$data, "pd_mapping")
  fit_dat <- workflow$data[workflow$data$S == 1, , drop = FALSE]
  pred <- OutPred(workflow$out_fo, fit_dat, workflow$data, 1, map)
  expect_true(all(pred >= 0 & pred <= 1))
})

test_that("SA supports binary outcomes with finite zero-noise estimates", {
  workflow <- make_pd_workflow(binary_outcome = TRUE)
  result <- suppressWarnings(SA(
    workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, 0
  ))
  expect_s3_class(result, "SA")
  expect_identical(result$settings$outcome_type, "B")
  expect_true(all(is.finite(result$data$estimate)))
})

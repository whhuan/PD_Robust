test_that("B = 0 returns point estimates without bootstrap rows", {
  workflow <- make_pd_workflow()
  sep <- HTESepT(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, 1, B = 0, verbose = FALSE)
  all <- HTEAllT(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, B = 0, verbose = FALSE)
  expect_equal(sep$bootstrap_info$requested, 0)
  expect_equal(sep$bootstrap_info$successful, 0)
  expect_equal(all$bootstrap_info$requested, 0)
  expect_equal(all$bootstrap_info$successful, 0)
  expect_true(is.matrix(sep$boot_mat))
  expect_true(is.matrix(all$boot_mat))
  expect_equal(nrow(sep$boot_mat), 0L)
  expect_equal(nrow(all$boot_mat), 0L)
  expect_equal(ncol(sep$boot_mat), nrow(sep$summary))
  expect_equal(ncol(all$boot_mat), nrow(all$summary))
  expect_identical(
    colnames(sep$boot_mat),
    paste(sep$summary$time, sep$summary$covariate, sep = "_")
  )
  expect_identical(colnames(all$boot_mat), all$summary$term)
  expect_true(all(is.na(sep$summary$SD)))
  expect_true(all(is.na(sep$summary$LowerBound)))
  expect_true(all(is.na(sep$summary$UpperBound)))
  expect_true(all(is.na(all$summary$SD)))
  expect_true(all(is.na(all$summary$LowerBound)))
  expect_true(all(is.na(all$summary$UpperBound)))
})

test_that("bootstrap defaults and argument validation are stable", {
  workflow <- make_pd_workflow()
  expect_error(HTESepT(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, 1, B = 2, max_attempts = 1, verbose = FALSE),
    "greater than or equal")
  expect_error(HTEAllT(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, B = 1.5, verbose = FALSE), "nonnegative integer")
})

test_that("cluster bootstrap preserves complete subject panels", {
  workflow <- make_pd_workflow(times = 0:2)
  set.seed(123)
  boot <- PDRobust:::.pd_cluster_bootstrap(workflow$data)
  counts <- table(boot$id)
  expect_true(all(counts == length(unique(workflow$data$time))))
  expect_equal(sort(unique(boot$time)), sort(unique(workflow$data$time)))
  expect_s3_class(boot, "pd_data")
})

test_that("ordinary warnings do not discard an otherwise usable result", {
  captured <- PDRobust:::.pd_capture_conditions({
    warning("diagnostic warning")
    7
  })
  expect_identical(captured$value, 7)
  expect_identical(captured$warnings, "diagnostic warning")
})

test_that("HTESepT bootstrap succeeds on the built-in binary data", {
  utils::data("BiSample", package = "PDRobust", envir = environment())
  map <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0,
    cutoff_time = 2,
    covariates = c("X1", "X2", "X4"),
    interest_vars = c("X1", "X2"),
    y_type = "B"
  )
  prepared <- DataStandard(BiSample, map)

  set.seed(20260729)
  result <- suppressWarnings(HTESepT(
    prepared,
    A ~ X1 + X2 + X4,
    S ~ X1 + X2 + X4 + A + time,
    Y ~ X1 + X2 + A,
    target_time = 1,
    B = 2,
    max_attempts = 20,
    verbose = FALSE
  ))

  expect_identical(result$bootstrap_info$requested, 2L)
  expect_identical(result$bootstrap_info$successful, 2L)
  expect_true(result$bootstrap_info$complete)
  expect_equal(nrow(result$boot_mat), 2L)
  expect_true(all(is.finite(result$boot_mat)))
  expect_named(
    result$bootstrap_info,
    c(
      "requested", "successful", "attempts", "complete", "failures",
      "failure_counts", "warnings", "warning_counts", "model_diagnostics"
    )
  )
  expect_named(
    result$bootstrap_info$failures,
    c("attempt", "category", "message")
  )
})

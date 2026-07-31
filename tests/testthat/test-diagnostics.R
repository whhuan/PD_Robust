test_that("PSDiag reproduces ordinary IPTW and SMD calculations", {
  workflow <- make_pd_workflow()
  result <- PSDiag(workflow$data, workflow$ps_fo)
  map <- attr(workflow$data, "pd_mapping")
  baseline <- workflow$data[workflow$data$time == map$baseline_time, , drop = FALSE]
  baseline <- baseline[order(baseline$id), , drop = FALSE]
  pi <- as.numeric(PDRobust:::.pd_pspred_impl(
    workflow$ps_fo, workflow$data, baseline, map
  ))
  pi <- pmin(pmax(pi, 0.01), 0.99)
  A <- baseline$A
  expected_weights <- A / pi + (1 - A) / (1 - pi)
  X <- baseline[c("X1", "X2", "X4")]
  expect_equal(result$propensity, pi, tolerance = 1e-12)
  expect_equal(result$weights, as.numeric(expected_weights), tolerance = 1e-12)
  expect_equal(result$smd_before, round(PDRobust:::.pd_smd(X, A), 3))
  expect_equal(result$smd_after,
    round(PDRobust:::.pd_smd(X, A, expected_weights), 3))
})

test_that("PrinSDiag uses cutoff-aligned principal probabilities", {
  workflow <- make_pd_workflow()
  result <- PrinSDiag(workflow$data, workflow$ps_fo, workflow$prin_fo)
  n_subjects <- length(unique(workflow$data$id))
  expect_length(result$propensity, n_subjects)
  expect_length(result$p0, n_subjects)
  expect_length(result$p1, n_subjects)
  expect_named(result$statistics, c("X1", "X2", "X4"))
  expect_true(all(result$p0 >= 0 & result$p0 <= 1))
  expect_true(all(result$p1 >= 0 & result$p1 <= 1))
})

test_that("diagnostics reject nonnumeric model covariates", {
  workflow <- make_pd_workflow()
  workflow$data$category <- factor(rep(c("a", "b"), length.out = nrow(workflow$data)))
  expect_error(PSDiag(workflow$data, A ~ category), "numeric covariates")
  expect_error(PrinSDiag(workflow$data, A ~ category,
    workflow$prin_fo), "numeric covariates")
})

test_that("PSDiag always clips propensity scores to 0.01 and 0.99", {
  workflow <- make_extreme_diagnostic_workflow()
  result <- suppressWarnings(PSDiag(workflow$data, workflow$ps_fo))

  expect_true(all(result$propensity >= 0.01))
  expect_true(all(result$propensity <= 0.99))
  expect_true(any(result$propensity == 0.01))
  expect_true(any(result$propensity == 0.99))
})

test_that("PrinSDiag always clips propensity scores to 0.01 and 0.99", {
  workflow <- make_extreme_diagnostic_workflow()
  result <- suppressWarnings(PrinSDiag(
    workflow$data, workflow$ps_fo, workflow$prin_fo
  ))

  expect_true(all(result$propensity >= 0.01))
  expect_true(all(result$propensity <= 0.99))
  expect_true(any(result$propensity == 0.01))
  expect_true(any(result$propensity == 0.99))
})

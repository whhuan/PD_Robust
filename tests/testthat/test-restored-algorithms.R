test_that("the SMD helper uses the original pooled and weighted ESS denominators", {
  X <- cbind(x = c(1, 2, 4, 8, 3, 6))
  A <- c(0, 0, 0, 1, 1, 1)
  w <- c(1, 2, 1, 1, 3, 2)

  x1 <- X[A == 1, , drop = FALSE]
  x0 <- X[A == 0, , drop = FALSE]
  n1 <- nrow(x1) - 1
  n0 <- nrow(x0) - 1
  m1 <- colMeans(x1)
  m0 <- colMeans(x0)
  v1 <- colSums((x1 - rep(m1, each = nrow(x1)))^2) / n1
  v0 <- colSums((x0 - rep(m0, each = nrow(x0)))^2) / n0
  expected_unweighted <- (m1 - m0) /
    sqrt((n1 * v1 + n0 * v0) / (n1 + n0))

  wt1 <- w * (A == 1)
  wt0 <- w * (A == 0)
  mw1 <- colSums(X * wt1) / sum(wt1)
  mw0 <- colSums(X * wt0) / sum(wt0)
  ssq1 <- colSums(wt1 * sweep(X, 2, mw1)^2)
  ssq0 <- colSums(wt0 * sweep(X, 2, mw0)^2)
  ess1 <- (sum(wt1)^2 - sum(wt1^2)) / sum(wt1)
  ess0 <- (sum(wt0)^2 - sum(wt0^2)) / sum(wt0)
  expected_weighted <- (mw1 - mw0) /
    sqrt((ssq1 + ssq0) / (ess1 + ess0))

  expect_equal(PDRobust:::.pd_smd(X, A), expected_unweighted)
  expect_equal(PDRobust:::.pd_smd(X, A, w), expected_weighted)
})

test_that("QR uses cutoff weighted intercept-only quantile regression", {
  workflow <- make_pd_workflow()
  result <- QR(
    workflow$data, workflow$prin_fo,
    quantile_level = c(0.25, 0.5, 0.75)
  )
  expect_s3_class(result, "QR")
  expect_length(result$quantile$X1, 3L)
})

test_that("ORCI supports either cutoff treatment group", {
  workflow <- make_pd_workflow()
  fit0 <- ORCI(
    workflow$data, S ~ X1 + X2 + X4,
    a = 0
  )
  fit1 <- ORCI(
    workflow$data, S ~ X1 + X2 + X4,
    a = 1
  )
  expect_equal(fit0$settings$a, 0)
  expect_equal(fit1$settings$a, 1)
  expect_true(all(fit0$analysis_data$A == 0))
  expect_true(all(fit1$analysis_data$A == 1))
  expect_error(
    ORCI(workflow$data, S ~ X1, a = 2),
    "exactly 0 or 1"
  )
})

test_that("SA uses the original all-observation variance at each time", {
  workflow <- make_pd_workflow()
  result <- SA(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
    ratiovec = 0
  )
  expected <- vapply(sort(unique(workflow$data$time)), function(t) {
    stats::var(workflow$data$Y[workflow$data$time == t], na.rm = TRUE)
  }, numeric(1))
  expect_equal(as.numeric(result$variance_by_time), round(expected, 3))
  expect_equal(sort(unique(result$data$time)), sort(unique(workflow$data$time)))
})

test_that("time-specific HTE reproduces the original estimating equations", {
  for (binary in c(FALSE, TRUE)) {
    workflow <- make_pd_workflow(times = 0:2, binary_outcome = binary)
    expected <- reference_htesep(workflow, c(0, 2))
    result <- PDRobust:::.pd_htesep_once(
      workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
      target_time = c(0, 2)
    )
    observed <- result$estimate
    expect_equal(observed, expected, tolerance = 1e-6)
  }
})

test_that("pooled HTE reproduces the original all-time equations", {
  for (binary in c(FALSE, TRUE)) {
    workflow <- make_pd_workflow(times = 0:2, binary_outcome = binary)
    expected <- reference_hteall(workflow)
    result <- PDRobust:::.pd_hteall_once(
      workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo
    )
    observed <- result$estimate
    expect_equal(observed, expected, tolerance = 1e-6)
  }
})

test_that("QR numerically matches weighted intercept-only quantile regression", {
  workflow <- make_pd_workflow()
  result <- QR(
    workflow$data, workflow$prin_fo,
    quantile_level = c(0.25, 0.5, 0.75)
  )
  components <- reference_hte_components(workflow)
  cutoff <- components$cutoff
  expected <- as.numeric(stats::coef(quantreg::rq(
    X1 ~ 1, data = cutoff, weights = components$p0,
    tau = c(0.25, 0.5, 0.75), method = "br", model = TRUE
  )))
  expect_equal(as.numeric(result$quantile$X1), round(expected, 3))
})

test_that("zero-noise SA reproduces the original time-specific equations", {
  for (binary in c(FALSE, TRUE)) {
    workflow <- make_pd_workflow(binary_outcome = binary)
    expected <- reference_htesep(workflow, 0:2)
    result <- suppressWarnings(PDRobust:::.pd_sa_impl(
      workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
      ratiovec = 0
    ))
    observed <- result$data[result$data$ratio == 0, , drop = FALSE]
    observed_matrix <- matrix(
      observed$estimate,
      nrow = 3, byrow = TRUE,
      dimnames = list(c("0", "1", "2"), colnames(expected))
    )
    expect_equal(observed_matrix, expected, tolerance = 1e-8)
  }
})

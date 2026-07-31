test_that("B = 0 does not consume random numbers", {
  workflow <- make_pd_workflow()
  set.seed(20260728)
  state_before <- .Random.seed

  invisible(HTESepT(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
    target_time = 1, B = 0, verbose = FALSE
  ))
  expect_identical(.Random.seed, state_before)

  invisible(HTEAllT(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
    B = 0, verbose = FALSE
  ))
  expect_identical(.Random.seed, state_before)
})

test_that("cluster bootstrap is reproducible under a fixed seed", {
  workflow <- make_pd_workflow(times = 0:2)
  set.seed(7301)
  first <- PDRobust:::.pd_cluster_bootstrap(workflow$data)
  set.seed(7301)
  second <- PDRobust:::.pd_cluster_bootstrap(workflow$data)

  expect_identical(first, second)
  expect_identical(attr(first, "pd_mapping"), attr(workflow$data, "pd_mapping"))
})

test_that("bootstrap standard errors and confidence limits are numerically correct", {
  estimate <- c(alpha = 1, beta = -2)
  bootstrap <- rbind(
    c(0, -1),
    c(1, -2),
    c(2, -3)
  )
  conf_level <- 0.95
  observed <- PDRobust:::.pd_bootstrap_summary(
    estimate, bootstrap, conf_level
  )
  expected_sd <- apply(bootstrap, 2, stats::sd)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  expect_equal(observed$sd, expected_sd)
  expect_equal(observed$lower, as.numeric(estimate) - z * expected_sd)
  expect_equal(observed$upper, as.numeric(estimate) + z * expected_sd)
})

test_that("DataCheck distinguishes structural and survivor outcome missingness", {
  raw <- make_pd_raw(n = 30, times = 0:2)
  map <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0, cutoff_time = 2,
    covariates = c("X1", "X2", "X4"),
    interest_vars = c("X1", "X2"), y_type = "C"
  )

  structural <- DataCheck(raw, map)
  structural_row <- structural$checks[
    structural$checks$check == "structural_outcome_missingness_after_death",
    , drop = FALSE
  ]
  expect_true(structural_row$passed)
  expect_identical(structural_row$severity, "information")

  alive_row <- which(raw$S == 1)[1]
  raw$Y[alive_row] <- NA_real_
  survivor_missing <- DataCheck(raw, map)
  survivor_row <- survivor_missing$checks[
    survivor_missing$checks$check == "outcome_missingness_among_survivors",
    , drop = FALSE
  ]
  expect_false(survivor_row$passed)
  expect_true(survivor_row$analysis_blocking)
  expect_true(alive_row %in% survivor_missing$diagnostics$outcome_missing_alive_rows)
})

test_that("bundled data complete an end-to-end point-estimation workflow", {
  utils::data("BiSample", package = "PDRobust", envir = environment())
  dat <- BiSample
  map <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = min(dat$time), cutoff_time = max(dat$time),
    covariates = c("X1", "X2", "X4"),
    interest_vars = c("X1", "X2"), y_type = "B"
  )
  prepared <- DataStandard(dat, map)
  ps_fo <- A ~ X1 + X2 + X4
  prin_fo <- S ~ X1 + X2 + X4 + A + time
  out_fo <- Y ~ X1 + X2 + A

  separate <- suppressWarnings(HTESepT(
    prepared, ps_fo, prin_fo, out_fo,
    target_time = max(prepared$time), B = 0, verbose = FALSE
  ))
  pooled <- suppressWarnings(HTEAllT(
    prepared, ps_fo, prin_fo, out_fo,
    B = 0, verbose = FALSE
  ))

  expect_s3_class(separate, "pd_hte_timevarying")
  expect_s3_class(pooled, "pd_hte_pooled")
  expect_true(all(is.finite(separate$summary$estimate)))
  expect_true(all(is.finite(pooled$summary$estimate)))
})

test_that("registered plot methods return ggplot objects invisibly", {
  workflow <- make_pd_workflow()
  objects <- list(
    PSDiag(workflow$data, workflow$ps_fo),
    PrinSDiag(workflow$data, workflow$ps_fo, workflow$prin_fo),
    ORCI(workflow$data, S ~ X1 + X2, a = 0),
    HTESepT(
      workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
      target_time = 1, B = 0, verbose = FALSE
    ),
    HTEAllT(
      workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
      B = 0, verbose = FALSE
    )
  )

  for (object in objects) {
    plotted <- NULL
    expect_no_warning({
      plotted <- withVisible(plot(object))
    })
    expect_false(plotted$visible)
    expect_s3_class(plotted$value, "ggplot")
  }
})

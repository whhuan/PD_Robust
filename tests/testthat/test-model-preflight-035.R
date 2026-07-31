test_that("prediction models reject unusable inputs and preserve their inputs", {
  workflow <- make_pd_workflow(n = 120)
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")
  original <- dat

  expect_error(
    PSPred(A ~ absent, dat, dat, map),
    "absent"
  )
  expect_error(
    OutPred(workflow$out_fo, dat, dat, a = 2, mapping = map),
    "`a`"
  )
  expect_error(
    OutPred(X1 ~ X2 + A, dat, dat, a = 1, mapping = map),
    "mapped outcome"
  )
  bad_prediction <- dat
  bad_prediction$X1[1L] <- NA_real_
  expect_error(
    OutPred(workflow$out_fo, dat, bad_prediction, 1, map),
    "missing|non-finite"
  )
  expect_identical(dat, original)
})

test_that("finite nuisance predictions survive low variation and aliasing", {
  workflow <- make_pd_workflow(n = 160)
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")

  zero <- dat
  zero$X1 <- 1
  zero_prediction <- suppressWarnings(
    PSPred(A ~ X1 + X4, zero, dat, map)
  )
  expect_true(all(is.finite(zero_prediction)))

  aliased <- dat
  aliased$X2 <- aliased$X1
  aliased_prediction <- suppressWarnings(
    PSPred(A ~ X1 + X2 + X4, aliased, dat, map)
  )
  expect_true(all(is.finite(aliased_prediction)))
})

test_that("separation is a warning when logistic predictions remain finite", {
  workflow <- make_pd_workflow(n = 160)
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")
  separated <- dat
  separated$A <- as.integer(separated$X1 > 0)

  prediction <- NULL
  expect_warning(
    prediction <- PSPred(A ~ X1, separated, dat, map),
    "fitting warning|separation"
  )
  expect_length(prediction, nrow(dat))
  expect_true(all(is.finite(prediction)))
})

test_that("OutPred retains finite binary predictions with one response level", {
  workflow <- make_pd_workflow(binary_outcome = TRUE, n = 160)
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")
  fit_dat <- dat[dat$S == 1, , drop = FALSE]
  fit_dat$Y <- 1L

  prediction <- NULL
  expect_warning(
    prediction <- OutPred(
      workflow$out_fo, fit_dat, dat, 1, map
    ),
    "one observed level|fitting warning|separation"
  )
  expect_true(all(is.finite(prediction)))
  expect_true(all(prediction >= 0 & prediction <= 1))
})

test_that("PrinPred warns but returns finite constant survival predictions", {
  workflow <- make_pd_workflow(times = 0:2, n = 160)
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")
  constant_survival <- dat
  constant_survival$S[constant_survival$time > map$baseline_time] <- 1L

  prediction <- NULL
  expect_warning(
    prediction <- PrinPred(
      workflow$prin_fo, constant_survival, dat, 0, map
    ),
    "one observed level|fitting warning|separation"
  )
  expect_true(all(is.finite(prediction)))
  expect_equal(
    prediction[dat$time == map$baseline_time],
    rep(1, length(unique(dat$id)))
  )
})

test_that("non-estimable HTE modifier systems remain errors", {
  workflow <- make_pd_workflow(n = 160)
  dat <- workflow$data
  dat$X2 <- dat$X1

  expect_error(
    HTESepT(
      dat, A ~ X1 + X4, S ~ X1 + X4 + A + time,
      Y ~ X1 + A, target_time = 1, B = 0, verbose = FALSE
    ),
    "rank deficient|not estimable"
  )
  expect_error(
    HTEAllT(
      dat, A ~ X1 + X4, S ~ X1 + X4 + A + time,
      Y ~ X1 + A, B = 0, verbose = FALSE
    ),
    "rank deficient|not estimable"
  )
})

test_that("ORCI omits aliased coefficients when estimable effects remain", {
  workflow <- make_pd_workflow(n = 240)
  dat <- workflow$data
  aliased <- dat
  aliased$X2 <- aliased$X1

  result <- NULL
  expect_warning(
    result <- ORCI(
      aliased, S ~ X1 + X2 + X4, a = 0
    ),
    "Aliased"
  )
  expect_s3_class(result, "odds_ratios")
  expect_gt(nrow(result$forestplotdat), 0L)
  expect_true(all(is.finite(result$forestplotdat$estcoef)))
})

test_that("diagnostics report non-estimable balance without model-fit errors", {
  workflow <- make_pd_workflow(n = 160)
  zero <- workflow$data
  zero$X1 <- 1

  result <- suppressWarnings(PSDiag(zero, A ~ X1))
  expect_s3_class(result, "PSDiag")
  expect_true(is.na(result$smd_before[["X1"]]))
  expect_true(is.na(result$smd_after[["X1"]]))
})

test_that("SA is reproducible for continuous and binary outcomes", {
  continuous <- make_pd_workflow(n = 160)
  original <- continuous$data
  set.seed(600)
  first <- SA(
    continuous$data, continuous$ps_fo, continuous$prin_fo,
    continuous$out_fo, ratiovec = c(0, 0.05)
  )
  set.seed(600)
  second <- SA(
    continuous$data, continuous$ps_fo, continuous$prin_fo,
    continuous$out_fo, ratiovec = c(0, 0.05)
  )
  expect_identical(first$data, second$data)
  expect_identical(continuous$data, original)
  expect_identical(first$settings$outcome_type, "C")

  binary <- make_pd_workflow(binary_outcome = TRUE, n = 200)
  binary_result <- suppressWarnings(SA(
    binary$data, binary$ps_fo, binary$prin_fo,
    binary$out_fo, ratiovec = 0
  ))
  expect_identical(binary_result$settings$outcome_type, "B")
  expect_true(all(is.finite(binary_result$data$estimate)))
})

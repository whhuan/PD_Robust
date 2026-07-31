test_that("prediction functions return pure aligned pd_prediction vectors", {
  workflow <- make_pd_workflow()
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")

  ps <- PSPred(workflow$ps_fo, dat, dat, map)
  p0 <- PrinPred(workflow$prin_fo, dat, dat, 0, map)
  p1 <- PrinPred(workflow$prin_fo, dat, dat, 1, map)
  mu <- OutPred(workflow$out_fo, dat[dat$S == 1, ], dat, 1, map)

  for (value in list(ps, p0, p1, mu)) {
    expect_true(is.numeric(value))
    expect_length(value, nrow(dat))
    expect_s3_class(value, "pd_prediction")
    expect_false(is.list(value))
  }
})

test_that("each prediction function refits from supplied fit data", {
  workflow <- make_pd_workflow()
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")

  ps1 <- PSPred(workflow$ps_fo, dat, dat, map)
  changed_ps <- dat
  baseline <- changed_ps$time == map$baseline_time
  changed_ps$A[baseline] <- 1L - changed_ps$A[baseline]
  ps2 <- PSPred(workflow$ps_fo, changed_ps, dat, map)
  expect_false(isTRUE(all.equal(as.numeric(ps1), as.numeric(ps2))))

  fit_y <- dat[dat$S == 1, ]
  mu1 <- OutPred(workflow$out_fo, fit_y, dat, 1, map)
  changed_y <- fit_y
  changed_y$Y <- changed_y$Y + changed_y$X1
  mu2 <- OutPred(workflow$out_fo, changed_y, dat, 1, map)
  expect_false(isTRUE(all.equal(as.numeric(mu1), as.numeric(mu2))))

  p1 <- PrinPred(workflow$prin_fo, dat, dat, 0, map)
  changed_s <- dat
  post <- changed_s$time > map$baseline_time
  changed_s$S[post] <- as.integer(
    (changed_s$id[post] + changed_s$time[post]) %% 4 != 0
  )
  changed_s$Y[changed_s$S == 0] <- NA
  p2 <- PrinPred(workflow$prin_fo, changed_s, dat, 0, map)
  expect_false(isTRUE(all.equal(as.numeric(p1), as.numeric(p2))))
})

test_that("principal scores accumulate over the complete baseline-to-cutoff grid", {
  workflow <- make_pd_workflow(times = 0:3)
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")
  score <- PrinPred(workflow$prin_fo, dat, dat, 0, map)

  expect_equal(score[dat$time == map$baseline_time], rep(1, length(unique(dat$id))))
  expect_true(all(score[dat$time == map$cutoff_time] <=
                    score[dat$time == 2] + 1e-12))
  expect_true(all(score[dat$time == 2] <= score[dat$time == 1] + 1e-12))
})

test_that("PrinPred follows the 0.2.3 baseline inclusion rule", {
  manual_principal <- function(workflow, treatment) {
    dat <- as.data.frame(workflow$data)
    map <- attr(workflow$data, "pd_mapping")
    dat$.order <- seq_len(nrow(dat))
    dat <- dat[order(dat[[map$id_col]], dat[[map$time_col]]), , drop = FALSE]
    post_times <- sort(unique(dat[[map$time_col]][
      dat[[map$time_col]] > map$baseline_time
    ]))
    observed_times <- sort(unique(dat[[map$time_col]]))

    if (length(observed_times) == 1L) {
      fit_rows <- rep(TRUE, nrow(dat))
    } else {
      groups <- split(seq_len(nrow(dat)), dat[[map$id_col]])
      ind <- integer(nrow(dat))
      for (idx in groups) {
        idx <- idx[order(dat[[map$time_col]][idx])]
        ind[idx[1L]] <- 0L
        previous <- dat[[map$S_col]][idx[-length(idx)]]
        ind[idx[-1L]] <- as.integer(!is.na(previous) & previous == 1)
      }
      fit_rows <- ind == 1L
    }
    model <- stats::glm(
      workflow$prin_fo, data = dat[fit_rows, , drop = FALSE],
      family = stats::binomial(link = "logit")
    )
    prediction_data <- dat
    prediction_data[[map$A_col]] <- treatment
    conditional <- as.numeric(stats::predict(
      model, newdata = prediction_data, type = "response"
    ))
    if (length(post_times)) {
      conditional[prediction_data[[map$time_col]] == map$baseline_time] <- 1
    }
    cumulative <- numeric(nrow(prediction_data))
    groups <- split(seq_len(nrow(prediction_data)), prediction_data[[map$id_col]])
    for (idx in groups) {
      idx <- idx[order(prediction_data[[map$time_col]][idx])]
      cumulative[idx] <- cumprod(conditional[idx])
    }
    cumulative[order(prediction_data$.order)]
  }

  for (times in list(0:2, 0:1, 0)) {
    workflow <- make_pd_workflow(times = times)
    map <- attr(workflow$data, "pd_mapping")
    observed <- PrinPred(
      workflow$prin_fo, workflow$data, workflow$data,
      treatment = 0, mapping = map
    )
    expected <- manual_principal(workflow, treatment = 0)
    expect_equal(as.numeric(observed), round(expected, 3))
  }
})

test_that("PrinPred uses no at-risk indicator for a single observed time", {
  workflow <- make_pd_workflow(times = 0)
  dat <- as.data.frame(workflow$data)
  map <- attr(workflow$data, "pd_mapping")

  manual_fit <- stats::glm(
    workflow$prin_fo, data = dat,
    family = stats::binomial(link = "logit")
  )
  prediction_data <- dat
  prediction_data[[map$A_col]] <- 0
  expected <- as.numeric(stats::predict(
    manual_fit, newdata = prediction_data, type = "response"
  ))
  observed <- PrinPred(
    workflow$prin_fo, workflow$data, workflow$data,
    treatment = 0, mapping = map
  )

  expect_equal(as.numeric(observed), round(expected, 3))
})

test_that("PrinPred rejects incomplete cumulative prediction grids", {
  workflow <- make_pd_workflow(times = 0:3)
  map <- attr(workflow$data, "pd_mapping")
  incomplete <- workflow$data[workflow$data$time != 2, , drop = FALSE]
  expect_error(
    PrinPred(
      workflow$prin_fo, workflow$data, incomplete,
      treatment = 0, mapping = map
    ),
    "every actual observed time"
  )
})

test_that("PSPred fits baseline only and predicts the complete pred_dat", {
  workflow <- make_pd_workflow(times = 0:2)
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")
  baseline <- dat[dat$time == map$baseline_time, , drop = FALSE]
  manual_fit <- stats::glm(
    workflow$ps_fo, data = baseline,
    family = stats::binomial(link = "logit")
  )
  expected <- as.numeric(stats::predict(
    manual_fit, newdata = dat, type = "response"
  ))
  observed <- PSPred(workflow$ps_fo, dat, dat, map)
  expect_equal(as.numeric(observed), round(expected, 3))
})

test_that("OutPred refits and predicts after fixing treatment and survival", {
  workflow <- make_pd_workflow(times = 0:2)
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")
  fit_dat <- as.data.frame(dat)
  fit_dat$Y[is.na(fit_dat$Y)] <-
    -2 + fit_dat$X1[is.na(fit_dat$Y)] - fit_dat$S[is.na(fit_dat$Y)]
  formula <- Y ~ X1 + X2 + A + S
  manual_fit <- stats::lm(formula, data = fit_dat)
  prediction_data <- as.data.frame(dat)
  prediction_data$A <- 1
  prediction_data$S <- 1
  expected <- as.numeric(stats::predict(
    manual_fit, newdata = prediction_data, type = "response"
  ))
  observed <- OutPred(formula, fit_dat, dat, a = 1, mapping = map)
  expect_equal(as.numeric(observed), round(expected, 3))
})

test_that("OutPred matches the original binary logistic prediction steps", {
  workflow <- make_pd_workflow(
    times = 0:2, binary_outcome = TRUE, n = 320
  )
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")
  fit_dat <- as.data.frame(dat[dat$S == 1, , drop = FALSE])
  fit_dat$Y[seq.int(1L, nrow(fit_dat), by = 31L)] <- NA_integer_
  formula <- Y ~ X1 + X2 + A + S

  manual_data <- fit_dat[!is.na(fit_dat$Y), , drop = FALSE]
  manual_fit <- suppressWarnings(stats::glm(
    formula,
    data = manual_data,
    family = stats::binomial(link = "logit")
  ))
  prediction_data <- as.data.frame(dat)
  prediction_data$A <- 0
  prediction_data$S <- 1
  expected <- suppressWarnings(as.numeric(stats::predict(
    manual_fit, newdata = prediction_data, type = "response"
  )))
  observed <- suppressWarnings(OutPred(
    formula, fit_dat, dat, a = 0, mapping = map
  ))

  expect_equal(as.numeric(observed), round(expected, 3))
  expect_true(all(observed >= 0 & observed <= 1))
})

test_that("OutPred permits aliased coefficients when predictions are finite", {
  workflow <- make_pd_workflow(n = 200)
  dat <- workflow$data
  map <- attr(dat, "pd_mapping")
  fit_dat <- as.data.frame(dat[dat$S == 1, , drop = FALSE])
  fit_dat$X2 <- fit_dat$X1

  prediction <- suppressWarnings(OutPred(
    Y ~ X1 + X2 + A,
    fit_dat,
    dat,
    a = 1,
    mapping = map
  ))
  expect_length(prediction, nrow(dat))
  expect_true(all(is.finite(prediction)))
})

test_that("OutPred preserves factor model matrices and rejects unseen levels", {
  workflow <- make_pd_workflow(n = 240)
  dat <- as.data.frame(workflow$data)
  map <- attr(workflow$data, "pd_mapping")
  dat$group <- factor(ifelse(dat$X4 > 0, "high", "low"))
  fit_dat <- dat[dat$S == 1, , drop = FALSE]
  formula <- Y ~ X1 + group + A + S

  manual_fit <- stats::lm(formula, data = fit_dat)
  prediction_data <- dat
  prediction_data$A <- 1
  prediction_data$S <- 1
  expected <- as.numeric(stats::predict(
    manual_fit, newdata = prediction_data, type = "response"
  ))
  observed <- OutPred(
    formula, fit_dat, dat, a = 1, mapping = map
  )
  expect_equal(as.numeric(observed), round(expected, 3))

  unseen <- dat
  unseen$group <- factor(
    as.character(unseen$group),
    levels = c(levels(dat$group), "new")
  )
  unseen$group[1L] <- "new"
  expect_error(
    OutPred(formula, fit_dat, unseen, a = 1, mapping = map),
    "Prediction failed"
  )
})

test_that("PrinPred test panels are full rank and avoid separation warnings", {
  for (times in list(0:3, 0)) {
    workflow <- make_pd_workflow(times = times, n = 320L)
    dat <- as.data.frame(workflow$data)
    map <- attr(workflow$data, "pd_mapping")
    dat <- dat[order(dat[[map$id_col]], dat[[map$time_col]]), , drop = FALSE]

    if (length(times) == 1L) {
      model_data <- dat
    } else {
      groups <- split(seq_len(nrow(dat)), dat[[map$id_col]])
      ind <- integer(nrow(dat))
      for (idx in groups) {
        idx <- idx[order(dat[[map$time_col]][idx])]
        ind[idx[1L]] <- 0L
        previous <- dat[[map$S_col]][idx[-length(idx)]]
        ind[idx[-1L]] <- as.integer(!is.na(previous) & previous == 1)
      }
      model_data <- dat[ind == 1L, , drop = FALSE]
    }

    design <- stats::model.matrix(workflow$prin_fo, data = model_data)
    expect_equal(qr(design)$rank, ncol(design))
    expect_setequal(unique(model_data[[map$S_col]]), c(0, 1))

    fitted_model <- NULL
    expect_no_warning({
      fitted_model <- stats::glm(
        workflow$prin_fo,
        data = model_data,
        family = stats::binomial(link = "logit")
      )
    })
    expect_true(fitted_model$converged)
    expect_true(all(is.finite(stats::coef(fitted_model))))

    expect_no_warning(
      PrinPred(
        workflow$prin_fo,
        workflow$data,
        workflow$data,
        treatment = 0,
        mapping = map
      )
    )
  }
})

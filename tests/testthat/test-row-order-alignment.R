test_that("prediction functions preserve pred_dat row order", {
  workflow <- make_pd_workflow(times = 0:2)
  map <- attr(workflow$data, "pd_mapping")
  set.seed(99)
  index <- sample(seq_len(nrow(workflow$data)))
  shuffled <- workflow$data[index, , drop = FALSE]

  ps_ref <- PSPred(workflow$ps_fo, workflow$data, workflow$data, map)
  ps_shuffled <- PSPred(workflow$ps_fo, workflow$data, shuffled, map)
  expect_equal(as.numeric(ps_shuffled), as.numeric(ps_ref)[index], tolerance = 1e-12)

  out_fit <- workflow$data[workflow$data$S == 1, , drop = FALSE]
  mu_ref <- OutPred(workflow$out_fo, out_fit, workflow$data, 1, map)
  mu_shuffled <- OutPred(workflow$out_fo, out_fit, shuffled, 1, map)
  expect_equal(as.numeric(mu_shuffled), as.numeric(mu_ref)[index], tolerance = 1e-12)
})

test_that("analysis estimates are invariant to panel row order", {
  workflow <- make_pd_workflow(times = 0:2)
  set.seed(100)
  shuffled <- workflow$data[sample(seq_len(nrow(workflow$data))), , drop = FALSE]
  ref <- HTESepT(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, c(0, 2), B = 0, verbose = FALSE)
  obs <- HTESepT(shuffled, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, c(0, 2), B = 0, verbose = FALSE)
  expect_equal(obs$summary$estimate, ref$summary$estimate, tolerance = 1e-8)
})

test_that("character subject IDs are handled without cross-subject accumulation", {
  raw <- make_pd_raw(n = 60, times = 0:2)
  raw$id <- paste0("subject-", raw$id)
  map <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0, cutoff_time = 2,
    covariates = c("X1", "X2", "X4"), interest_vars = c("X1", "X2"),
    y_type = "C")
  dat <- DataStandard(raw, map)
  scores <- PrinPred(S ~ X1 + X2 + X4 + A + time, dat, dat, 0,
    attr(dat, "pd_mapping"))
  expect_length(scores, nrow(dat))
  expect_true(all(scores >= 0 & scores <= 1))
})

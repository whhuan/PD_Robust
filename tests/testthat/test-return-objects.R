test_that("prediction functions return stable numeric prediction objects", {
  workflow <- make_pd_workflow()
  map <- attr(workflow$data, "pd_mapping")
  ps <- PSPred(workflow$ps_fo, workflow$data, workflow$data, map)
  pr <- PrinPred(workflow$prin_fo, workflow$data, workflow$data, 0, map)
  always <- workflow$data[workflow$data$S == 1, , drop = FALSE]
  mu <- OutPred(workflow$out_fo, always, workflow$data, 1, map)
  for (x in list(ps, pr, mu)) {
    expect_s3_class(x, "pd_prediction")
    expect_type(x, "double")
    expect_length(x, nrow(workflow$data))
    expect_true(all(is.finite(x)))
  }
})

test_that("diagnostic and analysis objects expose documented core fields", {
  workflow <- make_pd_workflow()
  psd <- PSDiag(workflow$data, workflow$ps_fo)
  prd <- PrinSDiag(workflow$data, workflow$ps_fo, workflow$prin_fo)
  qr <- QR(workflow$data, workflow$prin_fo, c(.25, .5, .75))
  or <- ORCI(workflow$data, S ~ X1 + X2, a = 0)
  sa <- SA(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, ratiovec = 0)
  sep <- HTESepT(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, target_time = c(0, 2), B = 0, verbose = FALSE)
  all <- HTEAllT(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, B = 0, verbose = FALSE)

  expect_named(psd, c("smd_before", "smd_after", "weights", "weight_type",
    "propensity", "data", "plot", "formula", "mapping", "call"))
  expect_named(prd, c("pripfigdat", "statistics", "propensity", "p0", "p1",
    "plot", "formulas", "mapping", "call"))
  expect_true(all(c("mean", "quantile", "weights", "mapping") %in% names(qr)))
  expect_true(all(c("forestplotdat", "model", "analysis_data", "settings") %in% names(or)))
  expect_true(all(c("data", "variance_by_time", "settings") %in% names(sa)))
  expect_true(all(c("summary", "bootstrap_info", "mapping") %in% names(sep)))
  expect_true(all(c("summary", "bootstrap_info", "analysis_times") %in% names(all)))
})

test_that("print methods return objects invisibly", {
  workflow <- make_pd_workflow()
  objects <- list(
    PSDiag(workflow$data, workflow$ps_fo),
    PrinSDiag(workflow$data, workflow$ps_fo, workflow$prin_fo),
    QR(workflow$data, workflow$prin_fo),
    ORCI(workflow$data, S ~ X1 + X2, a = 0),
    SA(workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo, 0),
    HTESepT(workflow$data, workflow$ps_fo, workflow$prin_fo,
      workflow$out_fo, 1, B = 0, verbose = FALSE),
    HTEAllT(workflow$data, workflow$ps_fo, workflow$prin_fo,
      workflow$out_fo, B = 0, verbose = FALSE)
  )
  for (object in objects) expect_invisible(print(object))
})

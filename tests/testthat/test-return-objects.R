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

test_that("print methods introduce results and draw stored user-facing plots", {
  workflow <- make_pd_workflow()
  mapping <- workflow$mapping
  check <- DataCheck(workflow$raw, mapping)
  diagnostic_ps <- PSDiag(workflow$data, workflow$ps_fo)
  diagnostic_principal <- PrinSDiag(
    workflow$data, workflow$ps_fo, workflow$prin_fo
  )
  odds <- ORCI(workflow$data, formula = S ~ X1 + X2, a = 0)
  sensitivity <- SA(
    workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, ratiovec = 0
  )
  separate <- HTESepT(
    workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, target_time = 1, B = 0, verbose = FALSE
  )
  pooled <- HTEAllT(
    workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, B = 0, verbose = FALSE
  )

  plot_calls <- 0L
  original_print_ggplot <- getS3method("print", "ggplot2::ggplot")
  registerS3method("print", "ggplot2::ggplot", function(x, ...) {
    plot_calls <<- plot_calls + 1L
    invisible(x)
  })
  on.exit(registerS3method(
    "print", "ggplot2::ggplot", original_print_ggplot
  ), add = TRUE)

  expect_output(print(mapping), "mapping and analysis settings[.]", fixed = FALSE)
  expect_output(print(check), "readiness summary[.]", fixed = FALSE)
  expect_output(print(diagnostic_ps), "before and after weighting[.]", fixed = FALSE)
  expect_output(print(diagnostic_principal), "diagnostic statistics[.]", fixed = FALSE)
  expect_output(print(odds), "confidence intervals[.]", fixed = FALSE)
  expect_output(print(QR(workflow$data, workflow$prin_fo)),
                "weighted means and quantiles[.]", fixed = FALSE)
  expect_output(print(sensitivity), "variance-ratio scenarios[.]", fixed = FALSE)
  expect_output(print(separate), "treatment-effect estimates[.]", fixed = FALSE)
  expect_output(print(pooled), "treatment-effect estimates[.]", fixed = FALSE)
  expect_identical(plot_calls, 5L + length(sensitivity$plot))
})

test_that("prediction and analysis functions honor custom structural names", {
  raw <- make_pd_raw(times = 0:2)
  names(raw)[match(c("id", "time", "A", "S", "Y"), names(raw))] <-
    c("subject", "visit", "treatment", "alive", "outcome")
  mapping <- Mapping(
    id = "subject", time = "visit", treatment = "treatment",
    survival = "alive", outcome = "outcome",
    baseline_time = 0, cutoff_time = 2,
    covariates = c("X1", "X2", "X4"),
    interest_vars = c("X1", "X2"), y_type = "C"
  )
  prepared <- DataStandard(raw, mapping)
  standardized_mapping <- attr(prepared, "pd_mapping")
  ps_fo <- treatment ~ X1 + X2 + X4
  prin_fo <- alive ~ X1 + X2 + X4 + treatment + visit
  out_fo <- outcome ~ X1 + X2 + treatment

  ps <- PSPred(ps_fo, prepared, prepared, standardized_mapping)
  p0 <- PrinPred(
    prin_fo, prepared, prepared, a = 0,
    mapping = standardized_mapping
  )
  fit_outcome <- prepared[prepared$alive == 1, , drop = FALSE]
  mu1 <- OutPred(
    out_fo, fit_outcome, prepared, a = 1,
    mapping = standardized_mapping
  )
  expect_length(ps, nrow(prepared))
  expect_length(p0, nrow(prepared))
  expect_length(mu1, nrow(prepared))

  expect_s3_class(PSDiag(prepared, ps_fo), "PSDiag")
  expect_s3_class(PrinSDiag(prepared, ps_fo, prin_fo), "PrinSDiag")
  expect_s3_class(QR(prepared, prin_fo), "QR")
  expect_s3_class(
    ORCI(prepared, alive ~ X1 + X2 + X4, a = 0),
    "odds_ratios"
  )
  expect_s3_class(
    SA(prepared, ps_fo, prin_fo, out_fo, ratiovec = 0),
    "SA"
  )
  expect_s3_class(
    HTESepT(
      prepared, ps_fo, prin_fo, out_fo,
      target_time = c(0, 2), B = 0, verbose = FALSE
    ),
    "pd_hte_timevarying"
  )
  expect_s3_class(
    HTEAllT(
      prepared, ps_fo, prin_fo, out_fo,
      B = 0, verbose = FALSE
    ),
    "pd_hte_pooled"
  )
})

test_that("Mapping rejects invalid structural definitions", {
  expect_error(Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 2, cutoff_time = 1,
    covariates = "X1", interest_vars = "X1", y_type = "C"),
    "must not be after")
  expect_error(Mapping(
    id = "id", time = "id", treatment = "A",
    survival = "S", outcome = "Y", baseline_time = 0,
    cutoff_time = 1, covariates = "X1", interest_vars = "X1", y_type = "C"),
    "different column")
  expect_error(Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0, cutoff_time = 1,
    covariates = "X1", interest_vars = "X2", y_type = "C"),
    "also be listed")
  expect_error(Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0, cutoff_time = 1,
    covariates = "X1", interest_vars = "X1", y_type = "unknown"),
    "continuous.*binary")
})

test_that("analysis functions require DataStandard output", {
  raw <- make_pd_raw()
  expect_error(PSDiag(raw, A ~ X1), "DataStandard")
  expect_error(PrinSDiag(raw, A ~ X1, S ~ X1 + A + time), "DataStandard")
  expect_error(QR(raw, S ~ X1 + A + time), "DataStandard")
  expect_error(ORCI(raw, S ~ X1), "DataStandard")
})

test_that("public analysis arguments are validated", {
  workflow <- make_pd_workflow()
  expect_error(QR(workflow$data, workflow$prin_fo, 0), "strictly between")
  expect_error(QR(workflow$data, workflow$prin_fo, 1), "strictly between")
  expect_error(ORCI(workflow$data, S ~ X1, a = 2), "exactly 0 or 1")
  expect_error(
    ORCI(workflow$data, S ~ X1, a = 0, conf_level = 1),
    "strictly between"
  )
  expect_error(SA(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, ratiovec = -0.1), "nonnegative")
  expect_error(HTESepT(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, target_time = 1, B = -1, verbose = FALSE),
    "nonnegative integer")
  expect_error(HTEAllT(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, B = 0, conf_level = 0, verbose = FALSE),
    "strictly between")
  expect_error(HTEAllT(workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, B = 0, verbose = NA), "`verbose`")
})

test_that("invalid formulas fail with informative errors", {
  workflow <- make_pd_workflow()
  expect_error(PSPred(A ~ missing_covariate, workflow$data, workflow$data,
    attr(workflow$data, "pd_mapping")), "missing_covariate")
  expect_error(OutPred(Y ~ missing_covariate, workflow$data, workflow$data,
    1, attr(workflow$data, "pd_mapping")), "missing_covariate")
})

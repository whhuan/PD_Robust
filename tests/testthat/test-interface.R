test_that("the public interface follows the independent-prediction design", {
  exports <- getNamespaceExports("PDRobust")
  expected <- c(
    "Mapping", "DataCheck", "DataStandard", "PSPred", "PrinPred",
    "OutPred", "HTESepT", "HTEAllT", "PSDiag", "PrinSDiag",
    "QR", "ORCI", "SA"
  )
  expect_true(all(expected %in% exports))
  expect_false("pd_example_data" %in% exports)
  expect_false(paste0("Exp", "Prob") %in% exports)
  expect_false(paste0("Mu", "Pred") %in% exports)
  expect_false(paste0("Nuisance", "Fit") %in% exports)
  expect_false(paste0("Nuisance", "Pre") %in% exports)

  map_args <- names(formals(Mapping))
  expect_false("target_time" %in% map_args)
  expect_true("target_time" %in% names(formals(HTESepT)))
  expect_false("target_time" %in% names(formals(HTEAllT)))
  expect_identical(tail(names(formals(HTESepT)), 1L), "progress_callback")
  expect_identical(tail(names(formals(HTEAllT)), 1L), "progress_callback")
  expect_identical(formals(HTESepT)$progress_callback, NULL)
  expect_identical(formals(HTEAllT)$progress_callback, NULL)
  expect_false("target_time" %in% names(formals(SA)))
  expect_false("target_time" %in% names(formals(PrinPred)))
  expect_identical(
    names(formals(PrinPred)),
    c("prin_fo", "fit_dat", "pred_dat", "a", "mapping", "...")
  )
  expect_identical(formals(PrinPred)$a, quote(expr = ))
  expect_false("treatment" %in% names(formals(PrinPred)))

  expect_identical(
    names(formals(ORCI)),
    c("data", "fomula", "a", "conf_level")
  )
  expect_identical(formals(ORCI)$a, quote(expr = ))
  expect_false("treatment_group" %in% names(formals(ORCI)))
})

test_that("Mapping has exactly ten required arguments", {
  expected <- c(
    "id", "time", "treatment", "survival", "outcome",
    "baseline_time", "cutoff_time", "covariates", "interest_vars", "y_type"
  )
  mapping_formals <- formals(Mapping)
  expect_identical(names(mapping_formals), expected)
  expect_true(all(vapply(
    mapping_formals,
    function(value) identical(value, quote(expr = )),
    logical(1)
  )))

  expect_error(
    Mapping(
      time = "time", treatment = "A", survival = "S", outcome = "Y",
      baseline_time = 0, cutoff_time = 2,
      covariates = c("X1", "X2"), interest_vars = "X1", y_type = "C"
    ),
    'argument "id" is missing'
  )

  mapping <- Mapping(
    id = "id", time = "time", treatment = "A",
    survival = "S", outcome = "Y",
    baseline_time = 0, cutoff_time = 2,
    covariates = c("X1", "X2"), interest_vars = "X1", y_type = "C"
  )
  expect_identical(
    unclass(mapping),
    list(
      id_col = "id", time_col = "time", A_col = "A",
      S_col = "S", Y_col = "Y", baseline_time = 0,
      cutoff_time = 2, covariates = c("X1", "X2"),
      interest_vars = "X1", y_type = "C"
    )
  )
})

test_that("ORCI requires explicit a and rejects the obsolete argument", {
  workflow <- make_pd_workflow()
  expect_error(
    ORCI(workflow$data, S ~ X1 + X2),
    'argument "a" is missing'
  )
  expect_error(
    ORCI(workflow$data, S ~ X1 + X2, treatment_group = 0),
    "unused argument"
  )
})

test_that("mapping never stores a target-time field", {
  workflow <- make_pd_workflow()
  expect_false("target_time" %in% names(workflow$mapping))
  expect_false("target_time" %in% names(attr(workflow$data, "pd_mapping")))
})

test_that("binary HTE public interfaces return finite mapped coefficients", {
  workflow <- make_pd_workflow(binary_outcome = TRUE)
  separate <- suppressWarnings(HTESepT(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
    target_time = 1, B = 0, verbose = FALSE
  ))
  pooled <- suppressWarnings(HTEAllT(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
    B = 0, verbose = FALSE
  ))
  expect_true(all(is.finite(separate$summary$estimate)))
  expect_true(all(is.finite(pooled$summary$estimate)))
})

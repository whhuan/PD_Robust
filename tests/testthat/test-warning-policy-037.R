test_that("checked logistic fits classify quasi-complete separation", {
  quasi <- data.frame(
    y = c(0L, 0L, 0L, 1L, 1L, 1L),
    x = c(-2, -1, 0, 0, 1, 2)
  )
  fit <- NULL
  expect_warning(
    fit <- PDRobust:::.pd_fit_glm_checked(
      y ~ x, quasi,
      label = "quasi-complete test model",
      strict = FALSE
    ),
    "separation"
  )
  expect_true(all(is.finite(stats::fitted(fit))))
  diagnostics <- attr(fit, "pd_model_diagnostics")
  expect_true(diagnostics$separation)
  expect_true(diagnostics$predictions_finite)
})

test_that("nonconvergence is distinct and reported once per checked fit", {
  workflow <- make_pd_workflow(n = 200L)
  separated <- workflow$data
  separated$A <- as.integer(separated$X1 > 0)
  map <- attr(separated, "pd_mapping")

  warnings <- character()
  prediction <- withCallingHandlers(
    PSPred(
      A ~ X1, separated, separated, map,
      control = stats::glm.control(maxit = 1L)
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_match(warnings, "did not converge")
  expect_true(all(is.finite(prediction)))
  diagnostics <- attr(prediction, "pd_model_diagnostics")
  expect_false(diagnostics$converged)
  expect_true(diagnostics$predictions_finite)
})

test_that("analysis calls consolidate repeated nuisance-model warnings", {
  workflow <- make_pd_workflow(n = 240L)
  separated <- workflow$data
  separated$A <- as.integer(separated$X1 > 0)

  captured <- PDRobust:::.pd_capture_conditions(
    HTESepT(
      separated, A ~ X1, workflow$prin_fo, workflow$out_fo,
      target_time = c(0, 1, 2), B = 0, verbose = FALSE
    )
  )
  expect_false(inherits(captured$value, "error"))
  expect_length(captured$warnings, 1L)
  expect_match(captured$warnings, "HTESepT.*nuisance-model instability")

  diagnostics <- captured$value$model_diagnostics
  warned <- diagnostics[nzchar(diagnostics$warning), , drop = FALSE]
  expect_gte(nrow(warned), 1L)
  expect_true(all(c(
    "analysis", "target_time", "treatment", "n_rows", "n_subjects",
    "response_0", "response_1", "formula", "predictors",
    "rank_deficient", "predictions_finite", "converged",
    "separation", "warning"
  ) %in% names(warned)))
})

test_that("counterfactual treatment predictions reuse one internal model fit", {
  workflow <- make_pd_workflow(n = 240L)

  separate <- PDRobust:::.pd_htesep_once(
    workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, target_time = c(0, 1, 2)
  )
  separate_labels <- table(separate$model_diagnostics$label)
  expect_identical(
    unname(separate_labels[["PrinPred principal-score model"]]), 1L
  )
  expect_identical(
    unname(separate_labels[["OutPred continuous-outcome model"]]), 3L
  )

  pooled <- PDRobust:::.pd_hteall_once(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo
  )
  pooled_labels <- table(pooled$model_diagnostics$label)
  expect_identical(
    unname(pooled_labels[["PrinPred principal-score model"]]), 1L
  )
  expect_identical(
    unname(pooled_labels[["OutPred continuous-outcome model"]]), 1L
  )
})

test_that("bootstrap model warnings are aggregated in returned diagnostics", {
  failures <- data.frame(
    attempt = integer(), category = character(), message = character(),
    stringsAsFactors = FALSE
  )
  warnings <- data.frame(
    attempt = c(1L, 1L, 2L),
    message = c("separation", "separation", "nonconvergence"),
    stringsAsFactors = FALSE
  )
  info <- PDRobust:::.pd_make_bootstrap_info(
    requested = 2L,
    successful = 2L,
    attempts = 2L,
    failures = failures,
    warnings = warnings
  )
  expect_equal(
    info$warning_counts$count[info$warning_counts$message == "separation"],
    2L
  )
  expect_equal(
    info$warning_counts$count[
      info$warning_counts$message == "nonconvergence"
    ],
    1L
  )
})

test_that("normal analysis data do not produce model warnings", {
  workflow <- make_pd_workflow(n = 320L)
  expect_no_warning(
    HTESepT(
      workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
      target_time = 1, B = 0, verbose = FALSE
    )
  )
  expect_no_warning(
    HTEAllT(
      workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
      B = 0, verbose = FALSE
    )
  )
  sa_result <- NULL
  expect_no_warning(
    sa_result <- SA(
      workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
      ratiovec = 0
    )
  )
  expect_true(all(sa_result$model_diagnostics$analysis == "SA"))
  expect_true(all(sa_result$model_diagnostics$sample == "original"))
})

at_most_three_decimals <- function(x) {
  x <- as.numeric(x)
  all(is.na(x) | abs(x * 1000 - round(x * 1000)) < 1e-8)
}

test_that("public predictions round only the full-precision return boundary", {
  workflow <- make_pd_workflow(n = 240L)
  map <- attr(workflow$data, "pd_mapping")
  outcome_fit <- workflow$data[workflow$data$S == 1L, , drop = FALSE]

  cases <- list(
    ps = list(
      full = PDRobust:::.pd_pspred_impl(
        workflow$ps_fo, workflow$data, workflow$data, map
      ),
      public = PSPred(
        workflow$ps_fo, workflow$data, workflow$data, map
      )
    ),
    principal = list(
      full = PDRobust:::.pd_prinpred_impl(
        workflow$prin_fo, workflow$data, workflow$data, 0, map
      ),
      public = PrinPred(
        workflow$prin_fo, workflow$data, workflow$data, 0, map
      )
    ),
    outcome = list(
      full = PDRobust:::.pd_outpred_impl(
        workflow$out_fo, outcome_fit, workflow$data, 1, map
      ),
      public = OutPred(
        workflow$out_fo, outcome_fit, workflow$data, 1, map
      )
    )
  )

  for (case in cases) {
    expect_equal(as.numeric(case$public), round(as.numeric(case$full), 3))
    expect_true(at_most_three_decimals(case$public))
    expect_true(any(
      abs(as.numeric(case$full) - round(as.numeric(case$full), 3)) > 1e-8
    ))
  }
})

test_that("HTE summaries round after full-precision estimation and inference", {
  workflow <- make_pd_workflow(n = 240L)

  full_sep <- PDRobust:::.pd_htesep_once(
    workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, target_time = c(0, 2)
  )
  public_sep <- HTESepT(
    workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, target_time = c(0, 2), B = 0, verbose = FALSE
  )
  expect_equal(
    public_sep$summary$estimate,
    round(as.numeric(t(full_sep$estimate)), 3)
  )

  full_all <- PDRobust:::.pd_hteall_once(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo
  )
  public_all <- HTEAllT(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
    B = 0, verbose = FALSE
  )
  expect_equal(
    public_all$summary$estimate,
    round(as.numeric(full_all$estimate), 3)
  )

  set.seed(3701)
  boot <- suppressWarnings(HTEAllT(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
    B = 3, max_attempts = 30, verbose = FALSE
  ))
  expect_identical(boot$bootstrap_info$successful, 3L)
  full_inference <- PDRobust:::.pd_bootstrap_summary(
    full_all$estimate, boot$boot_mat, boot$settings$conf_level
  )
  expect_equal(boot$summary$SD, unname(round(full_inference$sd, 3)))
  expect_equal(
    boot$summary$LowerBound,
    unname(round(full_inference$lower, 3))
  )
  expect_equal(
    boot$summary$UpperBound,
    unname(round(full_inference$upper, 3))
  )
  expect_true(any(abs(boot$boot_mat - round(boot$boot_mat, 3)) > 1e-8))
})

test_that("final analysis tables are rounded while internal quantities remain full", {
  workflow <- make_pd_workflow(n = 240L)
  psd <- PSDiag(workflow$data, workflow$ps_fo)
  prd <- PrinSDiag(workflow$data, workflow$ps_fo, workflow$prin_fo)
  qr_result <- QR(
    workflow$data, workflow$prin_fo, c(0.25, 0.5, 0.75)
  )
  or_result <- ORCI(workflow$data, S ~ X1 + X2 + X4, a = 0)
  sa_result <- SA(
    workflow$data, workflow$ps_fo, workflow$prin_fo,
    workflow$out_fo, ratiovec = c(0, 0.05)
  )

  expect_true(at_most_three_decimals(psd$smd_before))
  expect_true(at_most_three_decimals(psd$smd_after))
  expect_true(at_most_three_decimals(prd$statistics))
  expect_true(at_most_three_decimals(qr_result$mean))
  expect_true(all(vapply(
    qr_result$quantile, at_most_three_decimals, logical(1)
  )))
  expect_true(at_most_three_decimals(or_result$forestplotdat$estcoef))
  expect_true(at_most_three_decimals(or_result$forestplotdat$lowerbd))
  expect_true(at_most_three_decimals(or_result$forestplotdat$upperbd))
  expect_true(at_most_three_decimals(sa_result$data$estimate))

  expect_true(any(
    abs(psd$weights - round(psd$weights, 3)) > 1e-8
  ))
  expect_true(any(
    abs(psd$propensity - round(psd$propensity, 3)) > 1e-8
  ))
  expect_true(any(
    abs(stats::coef(or_result$model) -
          round(stats::coef(or_result$model), 3)) > 1e-8
  ))
})

test_that("standardized identifiers, times, and flags keep their storage modes", {
  workflow <- make_pd_workflow()
  check <- DataCheck(workflow$raw, workflow$mapping)
  expect_true(is.integer(workflow$data$id))
  expect_true(is.integer(workflow$data$time))
  expect_true(is.integer(workflow$data$A))
  expect_true(is.integer(workflow$data$S))
  expect_type(check$ready_for_analysis, "logical")
  expect_type(check$checks$passed, "logical")
})

test_that("data preparation preserves analysis-column precision", {
  workflow <- make_pd_workflow(n = 240L)
  expect_equal(workflow$data$X1, workflow$raw$X1, tolerance = 0)
  expect_equal(workflow$data$X2, workflow$raw$X2, tolerance = 0)
  expect_equal(workflow$data$Y, workflow$raw$Y, tolerance = 0)
  expect_true(any(
    abs(workflow$data$X1 - round(workflow$data$X1, 3)) > 1e-8
  ))

  imperfect <- workflow$raw
  imperfect$X1[1L] <- NA_real_
  internal <- PDRobust:::.pd_check_data_impl(
    imperfect, workflow$mapping, strict = FALSE
  )
  public <- DataCheck(imperfect, workflow$mapping)
  internal_percent <-
    internal$diagnostics$covariate_missing$affected_subject_percent
  public_percent <-
    public$diagnostics$covariate_missing$affected_subject_percent
  expect_equal(public_percent, round(internal_percent, 3))
  expect_true(any(
    abs(internal_percent - round(internal_percent, 3)) > 1e-8
  ))
})

test_that("bundled generated data contain at most three decimal places", {
  utils::data("BiSample", package = "PDRobust", envir = environment())
  utils::data(
    "ImperfectConSample", package = "PDRobust", envir = environment()
  )
  for (data in list(BiSample, ImperfectConSample)) {
    continuous_columns <- names(data)[vapply(data, is.double, logical(1))]
    expect_true(all(vapply(
      data[continuous_columns], at_most_three_decimals, logical(1)
    )))
  }
  expect_true(all(vapply(
    BiSample[c("id", "time", "A", "S", "Y")],
    is.integer,
    logical(1)
  )))
  expect_true(all(vapply(
    ImperfectConSample[
      c("patient_id", "visit_month", "treatment", "alive_status")
    ],
    is.character,
    logical(1)
  )))
})

test_that("the data generator rounds only its final returned data", {
  generator_path <- testthat::test_path(
    "..", "..", "data-raw", "generate_example_data.R"
  )
  if (!file.exists(generator_path)) {
    skip("data-raw generator source is unavailable")
  }

  expressions <- parse(generator_path)
  definitions <- vapply(expressions, function(expression) {
    is.call(expression) &&
      identical(expression[[1L]], as.name("<-")) &&
      identical(expression[[2L]], as.name("generate_data_example"))
  }, logical(1))
  expect_identical(sum(definitions), 1L)
  generator <- eval(expressions[[which(definitions)]][[3L]])
  body_lines <- deparse(body(generator), width.cutoff = 500L)
  output_line <- grep("output_dat <-", body_lines)
  round_line <- grep("round\\(value, digits = 3\\)", body_lines)
  return_line <- grep("dat = output_dat", body_lines)

  expect_length(output_line, 1L)
  expect_length(round_line, 1L)
  expect_length(return_line, 1L)
  expect_gt(round_line, output_line)
  expect_lt(round_line, return_line)
  expect_false(any(grepl(
    "round\\(",
    body_lines[seq_len(output_line - 1L)]
  )))
})

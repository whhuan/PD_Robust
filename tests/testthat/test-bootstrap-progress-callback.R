test_that("HTE progress callbacks expose the documented lifecycle", {
  workflow <- make_pd_workflow(n = 120L)

  all_updates <- list()
  all_result <- HTEAllT(
    workflow$data,
    workflow$ps_fo,
    workflow$prin_fo,
    workflow$out_fo,
    B = 0,
    verbose = FALSE,
    progress_callback = function(update) {
      all_updates[[length(all_updates) + 1L]] <<- update
    }
  )

  sep_updates <- list()
  sep_result <- HTESepT(
    workflow$data,
    workflow$ps_fo,
    workflow$prin_fo,
    workflow$out_fo,
    target_time = 1,
    B = 0,
    verbose = FALSE,
    progress_callback = function(update) {
      sep_updates[[length(sep_updates) + 1L]] <<- update
    }
  )

  required_fields <- c(
    "stage", "successful", "requested", "attempts", "max_attempts",
    "failed_attempts", "complete", "elapsed_seconds", "updated_at"
  )
  expect_s3_class(all_result, "pd_hte_pooled")
  expect_s3_class(sep_result, "pd_hte_timevarying")
  expect_true(length(all_updates) >= 3L)
  expect_true(length(sep_updates) >= 3L)
  expect_identical(vapply(all_updates, `[[`, character(1), "stage")[[1L]],
                   "initializing")
  expect_identical(vapply(all_updates, `[[`, character(1), "stage")[[length(all_updates)]],
                   "completed")
  expect_identical(vapply(sep_updates, `[[`, character(1), "stage")[[1L]],
                   "initializing")
  expect_identical(vapply(sep_updates, `[[`, character(1), "stage")[[length(sep_updates)]],
                   "completed")
  expect_named(all_updates[[1L]], required_fields)
  expect_named(sep_updates[[1L]], required_fields)
  expect_identical(all_updates[[length(all_updates)]]$complete, TRUE)
  expect_identical(sep_updates[[length(sep_updates)]]$complete, TRUE)
})

test_that("progress callbacks report bootstrap attempts", {
  workflow <- make_pd_workflow(n = 120L)
  updates <- list()

  set.seed(20260802)
  result <- suppressWarnings(HTEAllT(
    workflow$data,
    workflow$ps_fo,
    workflow$prin_fo,
    workflow$out_fo,
    B = 1,
    max_attempts = 10,
    verbose = FALSE,
    progress_callback = function(update) {
      updates[[length(updates) + 1L]] <<- update
    }
  ))

  final <- updates[[length(updates)]]
  expect_identical(final$stage, "completed")
  expect_identical(final$successful, result$bootstrap_info$successful)
  expect_identical(final$attempts, result$bootstrap_info$attempts)
  expect_identical(
    final$failed_attempts,
    result$bootstrap_info$attempts - result$bootstrap_info$successful
  )
  expect_true(any(vapply(updates, `[[`, character(1), "stage") == "bootstrap"))
})

test_that("invalid or failing progress callbacks are isolated", {
  workflow <- make_pd_workflow(n = 120L)

  expect_error(
    HTEAllT(
      workflow$data,
      workflow$ps_fo,
      workflow$prin_fo,
      workflow$out_fo,
      B = 0,
      verbose = FALSE,
      progress_callback = "not a function"
    ),
    "progress_callback"
  )

  calls <- 0L
  result <- NULL
  expect_warning(
    result <- HTEAllT(
      workflow$data,
      workflow$ps_fo,
      workflow$prin_fo,
      workflow$out_fo,
      B = 0,
      verbose = FALSE,
      progress_callback = function(update) {
        calls <<- calls + 1L
        stop("monitor unavailable")
      }
    ),
    "progress callback was disabled"
  )
  expect_s3_class(result, "pd_hte_pooled")
  expect_identical(calls, 1L)
})

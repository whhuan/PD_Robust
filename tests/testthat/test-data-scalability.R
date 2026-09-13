test_that("grouped visit counts preserve duplicate, missing and off-grid semantics", {
  keys <- c("10", "2", "10", "2", "10", "z", "z", NA, "2")
  time <- c(2, 0, 0, 0, 2, NA, 1, 0, 99)
  groups <- split(seq_along(keys), keys)
  codes <- as.integer(factor(keys))
  for (times in list(0:2, c(0, 2), 99, c(0.001, 2))) {
    result <- .pd_panel_visits(codes, time, times, length(groups))
    expect_identical(result$complete, unname(vapply(groups, function(i) {
      all(times %in% time[i])
    }, logical(1))))
    expect_identical(result$missing, vapply(times, function(t) {
      sum(!vapply(groups, function(i) t %in% time[i], logical(1)))
    }, integer(1)))
  }
  keep <- !duplicated(data.frame(keys, time))
  expect_identical(
    .pd_panel_visits(codes[keep], time[keep], 0:2, length(groups), unique_pairs = TRUE),
    .pd_panel_visits(codes, time, 0:2, length(groups))
  )
})

test_that("survival diagnostics preserve stable tied and missing time ordering", {
  w <- make_pd_workflow(n = 40L)
  raw <- w$raw
  raw$time[1:9] <- c(1, 1, NA, 0, NA, 0, 2, NA, 1)
  raw$S[1:9] <- c(0, 1, NA, 0, 1, 1, 0, 1, NA)
  raw$id[1:9] <- c(10, 10, 10, 2, 2, 2, 3, 3, 3)
  groups <- split(seq_len(nrow(raw)), as.character(raw$id))
  expected <- names(groups)[vapply(groups, function(i) {
    s <- stats::na.omit(raw$S[i[order(raw$time[i], na.last = TRUE)]])
    length(s) > 1L && any(diff(s) > 0)
  }, logical(1))]
  expect_identical(DataCheck(raw, w$mapping)$diagnostics$impossible_survival_transitions,
                   expected)
})

test_that("attrition retains sorted reasons and encounter-ordered incomplete IDs", {
  w <- make_pd_workflow(n = 40L)
  raw <- w$raw
  raw$id <- paste0("subject-", raw$id)
  raw <- raw[!(raw$id == "subject-2" & raw$time == 1), ]
  raw$X1[raw$id == "subject-10"] <- NA_real_
  raw$X2[raw$id == "subject-2"] <- NA_real_
  prepared <- DataStandard(raw, w$mapping, drop = TRUE)
  reasons <- attr(prepared, "pd_standardization")$attrition$removed_subjects_by_reason
  expect_identical(reasons, data.frame(
    subject = c("subject-2", "subject-10", "subject-2"),
    reason = c("missing_analysis_visit", rep("missing_required_analysis_value", 2))
  ))
  expect_identical(attr(prepared, "pd_mapping"), w$mapping)
  expect_identical(prepared$id, rep(seq_len(38L), each = 3L))
  expect_identical(attr(prepared, "pd_check")$ready_for_analysis, TRUE)
})

test_that("empty-string IDs keep legacy completeness diagnostics", {
  w <- make_pd_workflow(n = 40L)
  raw <- w$raw
  raw$id <- ifelse(raw$id == 1, "", as.character(raw$id))
  check <- DataCheck(raw, w$mapping)
  # Named [[ lookup/assignment for "" in the original implementation marks
  # both its original and appended entries incomplete, despite observed visits.
  expect_identical(check$diagnostics$incomplete_subjects, c("", ""))
  expect_identical(check$diagnostics$missing_by_time$missing_subjects, rep(0L, 3))
})

test_that("factor covariates preserve attrition and their original levels", {
  w <- make_pd_workflow(n = 40L)
  plain <- w$raw
  plain$X4[plain$id == 3L] <- NA
  classed <- plain
  classed$X4 <- factor(classed$X4, levels = c(0, 1))

  prepared_plain <- DataStandard(plain, w$mapping, drop = TRUE)
  prepared_classed <- DataStandard(classed, w$mapping, drop = TRUE)
  expect_identical(
    attr(prepared_classed, "pd_standardization")$attrition,
    attr(prepared_plain, "pd_standardization")$attrition
  )
  expect_identical(prepared_classed$id, prepared_plain$id)
  expect_identical(prepared_classed$time, prepared_plain$time)
  expect_identical(as.character(prepared_classed$X4),
                   as.character(prepared_plain$X4))
  expect_identical(levels(prepared_classed$X4), levels(classed$X4))
  expect_true(attr(prepared_classed, "pd_check")$ready_for_analysis)
})

test_that("the final check prevents analysis after deletion removes one arm", {
  w <- make_pd_workflow(n = 40L)
  raw <- w$raw
  raw$X1[raw$A == 1L] <- NA_real_
  initial <- DataCheck(raw, w$mapping)
  expect_true(initial$can_standardize)
  expect_false(initial$ready_for_analysis)

  expect_warning(
    prepared <- DataStandard(raw, w$mapping, drop = TRUE),
    "not ready for analysis: treatment_group_availability"
  )
  expect_false(attr(prepared, "pd_check")$ready_for_analysis)
  expect_identical(unique(prepared$A), 0L)
  expect_error(
    HTEAllT(prepared, w$ps_fo, w$prin_fo, w$out_fo, B = 0, verbose = FALSE),
    "not marked ready for analysis"
  )
})

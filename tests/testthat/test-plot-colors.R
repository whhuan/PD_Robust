test_that("HTEAllT maps distinct stable colors to reported terms", {
  workflow <- make_pd_workflow()
  result <- HTEAllT(
    workflow$data, workflow$ps_fo, workflow$prin_fo, workflow$out_fo,
    B = 0, verbose = FALSE
  )
  built <- ggplot2::ggplot_build(result$forest_plot)
  point_layer <- built$data[[2L]]

  expect_equal(
    length(unique(point_layer$colour)),
    nrow(result$summary)
  )
  expect_match(
    paste(deparse(result$forest_plot$mapping$colour), collapse = ""),
    "term"
  )
  expect_identical(
    result$forest_plot$labels$colour,
    "Term"
  )
})

test_that("ORCI uses the same variable colors for intervals and points", {
  workflow <- make_pd_workflow(n = 240)
  result <- ORCI(
    workflow$data, S ~ X1 + X2 + X4, a = 0
  )
  built <- ggplot2::ggplot_build(result$plot)
  interval_layer <- built$data[[2L]]
  point_layer <- built$data[[3L]]

  expect_equal(
    length(unique(point_layer$colour)),
    nrow(result$forestplotdat)
  )
  expect_setequal(
    unique(interval_layer$colour),
    unique(point_layer$colour)
  )
  expect_identical(result$plot$labels$colour, "Covariate")
})

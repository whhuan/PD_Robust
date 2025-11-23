test_that("PrinPred runs without error", {
  skip_if_not_installed("speedglm")

  df <- data.frame(
    y   = rbinom(50, 1, 0.5),
    A   = rbinom(50, 1, 0.5),
    x1  = rnorm(50),
    x2 = rbinom(50, 1, 0.5),
    ind = 1L
  )
  pred_dat <- data.frame(x1 = c(-1, 0, 1))

  fo <- y ~ A + x1 + x2

  res <- PrinPred(fo, fit_dat = df, pred_dat = pred_dat, trt = 1)
  expect_type(res, "list")
  expect_true("e_pred" %in% names(res))
  expect_equal(length(res$e_pred), nrow(pred_dat))
})

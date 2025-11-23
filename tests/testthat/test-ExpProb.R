test_that("ExpProb runs without error", {
  data <- data.frame(
    A = c(1, 0, 1, 0),
    X1 = c(0.5, 0.3, 0.8, 0.1),
    X2 = c(1, 0, 1, 1)
  )

  prob <- ExpProb(A ~ X1 + X2, fit_dat = data, pred_dat = data)

  expect_type(prob, "double")
  expect_true(all(prob >= 0 & prob <= 1))
  expect_length(prob, nrow(data))
})


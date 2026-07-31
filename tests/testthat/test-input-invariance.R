test_that("public functions do not mutate their inputs", {
  workflow <- make_pd_workflow()
  original_data <- workflow$data
  original_mapping <- attr(workflow$data, "pd_mapping")
  map <- original_mapping

  invisible(PSPred(workflow$ps_fo, workflow$data, workflow$data, map))
  invisible(PrinPred(workflow$prin_fo, workflow$data, workflow$data, 0, map))
  invisible(PSDiag(workflow$data, workflow$ps_fo))
  invisible(PrinSDiag(workflow$data, workflow$ps_fo, workflow$prin_fo))
  expect_identical(workflow$data, original_data)
  expect_identical(attr(workflow$data, "pd_mapping"), original_mapping)
})

test_that("results do not depend on prior calls with other data", {
  first <- make_pd_workflow(times = 0:2)
  second <- make_pd_workflow(times = 0:1)
  map1 <- attr(first$data, "pd_mapping")
  before <- PSPred(first$ps_fo, first$data, first$data, map1)
  invisible(PSPred(second$ps_fo, second$data, second$data,
    attr(second$data, "pd_mapping")))
  after <- PSPred(first$ps_fo, first$data, first$data, map1)
  expect_equal(after, before, tolerance = 0)
})

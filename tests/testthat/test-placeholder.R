test_that("package loads successfully", {
  expect_true(requireNamespace("rdborrow", quietly = TRUE))
})

test_that("SyntheticData is available", {
  data("SyntheticData", package = "rdborrow", envir = environment())
  expect_true(exists("SyntheticData", envir = environment()))
})

test_that("simulate_trt_assign returns a data frame with column A", {
  X <- data.frame(x1 = rnorm(10))
  S <- data.frame(S = rep(1, 10))
  result <- simulate_trt_assign(X, S, prob = 0.5)
  expect_s3_class(result, "data.frame")
  expect_named(result, "A")
  expect_identical(nrow(result), 10L)
})

test_that("simulate_trt_assign gives A = 0 for all external controls", {
  X <- data.frame(x1 = rnorm(20))
  S <- data.frame(S = rep(0, 20))
  result <- simulate_trt_assign(X, S, prob = 0.5)
  expect_identical(result$A, rep(0, 20))
})

test_that("simulate_trt_assign gives A = 1 for all RCT patients when prob = 1", {
  X <- data.frame(x1 = rnorm(20))
  S <- data.frame(S = rep(1, 20))
  result <- simulate_trt_assign(X, S, prob = 1)
  expect_identical(result$A, rep(1, 20))
})

test_that("simulate_trt_assign gives A = 0 for all RCT patients when prob = 0", {
  X <- data.frame(x1 = rnorm(20))
  S <- data.frame(S = rep(1, 20))
  result <- simulate_trt_assign(X, S, prob = 0)
  expect_identical(result$A, rep(0, 20))
})

test_that("simulate_trt_assign respects mixed S values", {
  set.seed(42)
  X <- data.frame(x1 = rnorm(100))
  S <- data.frame(S = c(rep(1, 50), rep(0, 50)))
  result <- simulate_trt_assign(X, S, prob = 0.5)
  expect_identical(result$A[51:100], rep(0, 50))
  expect_gt(sum(result$A[1:50]), 0)
  expect_lt(sum(result$A[1:50]), 50)
})

test_that("simulate_trt_assign validates prob range", {
  X <- data.frame(x1 = 1)
  S <- data.frame(S = 1)
  expect_error(simulate_trt_assign(X, S, prob = -0.1))
  expect_error(simulate_trt_assign(X, S, prob = 1.1))
})

test_that("simulate_trt_assign validates X is a data frame", {
  expect_error(simulate_trt_assign("not_df", data.frame(S = 1), prob = 0.5))
})

test_that("simulate_trt_assign errors when X and S have different row counts", {
  X <- data.frame(x1 = rnorm(10))
  S <- data.frame(S = rep(1, 5))
  expect_error(simulate_trt_assign(X, S, prob = 0.5), "same number of rows")
})

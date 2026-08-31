test_that("simulate_trial_status returns a data frame with column S", {
  X <- data.frame(x1 = rnorm(10))
  result <- simulate_trial_status(X, list(family = "binomial", coef = c(0, 0.5)))
  expect_s3_class(result, "data.frame")
  expect_named(result, "S")
  expect_identical(nrow(result), 10L)
})

test_that("simulate_trial_status returns only 0s and 1s", {
  X <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
  result <- simulate_trial_status(X, list(family = "binomial", coef = c(0, 1, -1)))
  expect_identical(sort(unique(result$S)), c(0L, 1L))
})

test_that("simulate_trial_status respects coefficients", {
  set.seed(42)
  X <- data.frame(x1 = rnorm(5000))
  # large positive intercept -> most should be S = 1
  result_high <- simulate_trial_status(X, list(family = "binomial", coef = c(5, 0)))
  expect_gt(mean(result_high$S), 0.9)
  # large negative intercept -> most should be S = 0
  result_low <- simulate_trial_status(X, list(family = "binomial", coef = c(-5, 0)))
  expect_lt(mean(result_low$S), 0.1)
})

test_that("simulate_trial_status works with multiple covariates", {
  X <- data.frame(x1 = rnorm(50), x2 = rnorm(50), x3 = rnorm(50))
  result <- simulate_trial_status(X, list(family = "binomial", coef = c(0, 0.5, -0.5, 0.1)))
  expect_identical(nrow(result), 50L)
})

test_that("simulate_trial_status errors on unsupported family", {
  X <- data.frame(x1 = rnorm(10))
  expect_error(
    simulate_trial_status(X, list(family = "gaussian", coef = c(0, 0.5)))
  )
})

test_that("simulate_trial_status errors when coef length mismatches X", {
  X <- data.frame(x1 = rnorm(10), x2 = rnorm(10))
  expect_error(
    simulate_trial_status(X, list(family = "binomial", coef = c(0, 0.5)))
  )
})

test_that("simulate_trial_status validates X is a data frame", {
  expect_error(
    simulate_trial_status("not_df", list(family = "binomial", coef = c(0)))
  )
})

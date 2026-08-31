test_that("simulate_X_dct_mvnorm returns correct dimensions", {
  result <- simulate_X_dct_mvnorm(50, 3)
  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 50L)
  expect_identical(ncol(result), 3L)
})

test_that("simulate_X_dct_mvnorm names columns x1 through xp", {
  result <- simulate_X_dct_mvnorm(10, 4)
  expect_named(result, c("x1", "x2", "x3", "x4"))
})

test_that("simulate_X_dct_mvnorm respects mu and sig", {
  set.seed(42)
  result <- simulate_X_dct_mvnorm(
    5000, 2,
    mu = c(100, -50),
    sig = diag(c(1, 0.25))
  )
  expect_gt(mean(result$x1), 99)
  expect_lt(mean(result$x1), 101)
  expect_gt(mean(result$x2), -51)
  expect_lt(mean(result$x2), -49)
})

test_that("simulate_X_dct_mvnorm discretizes cat_cols", {
  set.seed(42)
  result <- simulate_X_dct_mvnorm(
    100, 3,
    cat_cols = c(1),
    cat_prob = list(c(0.3, 0.7))
  )
  expect_identical(sort(unique(result$x1)), c(0, 1))
  expect_type(result$x2, "double")
})

test_that("simulate_X_dct_mvnorm handles multiple categorical columns", {
  set.seed(42)
  result <- simulate_X_dct_mvnorm(
    200, 3,
    cat_cols = c(1, 3),
    cat_prob = list(c(0.2, 0.6, 0.2), c(0.3, 0.7))
  )
  expect_identical(sort(unique(result$x1)), c(0, 1, 2))
  expect_identical(sort(unique(result$x3)), c(0, 1))
  expect_type(result$x2, "double")
})

test_that("simulate_X_dct_mvnorm works with no categorical columns", {
  result <- simulate_X_dct_mvnorm(20, 2)
  expect_identical(nrow(result), 20L)
  expect_identical(ncol(result), 2L)
})

test_that("simulate_X_dct_mvnorm validates n", {
  expect_error(simulate_X_dct_mvnorm(0, 2))
  expect_error(simulate_X_dct_mvnorm(-1, 2))
})

test_that("simulate_X_dct_mvnorm validates mu length", {
  expect_error(simulate_X_dct_mvnorm(10, 3, mu = c(0, 0)))
})

test_that("simulate_X_dct_mvnorm validates sig dimensions", {
  expect_error(simulate_X_dct_mvnorm(10, 3, sig = diag(2)))
})

test_that("simulate_X_dct_mvnorm validates cat_cols range", {
  expect_error(simulate_X_dct_mvnorm(
    10, 2,
    cat_cols = c(3),
    cat_prob = list(c(0.5, 0.5))
  ))
})

test_that("simulate_X_dct_mvnorm validates cat_prob length matches cat_cols", {
  expect_error(simulate_X_dct_mvnorm(
    10, 3,
    cat_cols = c(1, 2),
    cat_prob = list(c(0.5, 0.5))
  ))
})

test_that("simulate_outcome_from_model returns correct dimensions", {
  X <- data.frame(x1 = rnorm(20), x2 = rnorm(20))
  A <- rbinom(20, 1, 0.5)
  specs <- list(
    list(
      effect = 0, model_form_x = c("1" = 1, "x1" = 0.5, "x2" = -0.3),
      noise_mean = 0, noise_sd = 1
    ),
    list(
      effect = 0, model_form_x = c("1" = 2, "x1" = 0.1, "x2" = 0.2),
      noise_mean = 0, noise_sd = 1
    )
  )
  result <- simulate_outcome_from_model(X, A, specs, OLE_flag = FALSE, T_cross = 2)
  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 20L)
  expect_named(result, c("y1", "y2"))
})

test_that("simulate_outcome_from_model applies treatment effect", {
  set.seed(42)
  n <- 5000
  X <- data.frame(x1 = rep(0, n))
  A_treat <- rep(1, n)
  A_ctrl <- rep(0, n)
  specs <- list(
    list(
      effect = 10, model_form_x = c("1" = 0, "x1" = 0),
      noise_mean = 0, noise_sd = 0.1
    )
  )
  Y_treat <- simulate_outcome_from_model(X, A_treat, specs, OLE_flag = FALSE, T_cross = 1)
  Y_ctrl <- simulate_outcome_from_model(X, A_ctrl, specs, OLE_flag = FALSE, T_cross = 1)
  expect_gt(mean(Y_treat$y1), 9)
  expect_lt(mean(Y_ctrl$y1), 1)
})

test_that("simulate_outcome_from_model uses all covariate coefficients", {
  set.seed(42)
  n <- 5000
  X <- data.frame(x1 = rep(1, n), x2 = rep(1, n))
  A <- rep(0, n)
  specs <- list(
    list(
      effect = 0, model_form_x = c("1" = 5, "x1" = 3, "x2" = 2),
      noise_mean = 0, noise_sd = 0.01
    )
  )
  result <- simulate_outcome_from_model(X, A, specs, OLE_flag = FALSE, T_cross = 1)
  expect_gt(mean(result$y1), 9.5)
  expect_lt(mean(result$y1), 10.5)
})

test_that("simulate_outcome_from_model OLE phase uses trt = 1 for all", {
  set.seed(42)
  n <- 5000
  X <- data.frame(x1 = rep(0, n))
  A <- rep(0, n)
  specs <- list(
    list(
      effect = 0, model_form_x = c("1" = 0, "x1" = 0),
      noise_mean = 0, noise_sd = 0.1
    ),
    list(
      effect = 10, model_form_x = c("1" = 0, "x1" = 0),
      noise_mean = 0, noise_sd = 0.1
    )
  )
  # primary phase: A = 0, so effect not applied at t = 1
  # OLE phase (t = 2 > T_cross = 1): effect applied regardless of A
  result <- simulate_outcome_from_model(X, A, specs, OLE_flag = TRUE, T_cross = 1)
  expect_lt(abs(mean(result$y1)), 1)
  expect_gt(mean(result$y2), 9)
})

test_that("simulate_outcome_from_model primary phase respects A in OLE mode", {
  set.seed(42)
  n <- 5000
  X <- data.frame(x1 = rep(0, n))
  A <- rep(0, n)
  specs <- list(
    list(
      effect = 10, model_form_x = c("1" = 0, "x1" = 0),
      noise_mean = 0, noise_sd = 0.1
    ),
    list(
      effect = 10, model_form_x = c("1" = 0, "x1" = 0),
      noise_mean = 0, noise_sd = 0.1
    )
  )
  result <- simulate_outcome_from_model(X, A, specs, OLE_flag = TRUE, T_cross = 1)
  # t = 1: A = 0, so no effect
  expect_lt(abs(mean(result$y1)), 1)
  # t = 2: OLE, trt = 1 for all
  expect_gt(mean(result$y2), 9)
})

test_that("simulate_outcome_from_model validates inputs", {
  X <- data.frame(x1 = rnorm(5))
  A <- rbinom(5, 1, 0.5)
  specs <- list(
    list(
      effect = 0, model_form_x = c("1" = 1, "x1" = 0.5),
      noise_mean = 0, noise_sd = 1
    )
  )
  expect_error(simulate_outcome_from_model("bad", A, specs, FALSE, 1))
  expect_error(simulate_outcome_from_model(X, c(1, 2), specs, FALSE, 1))
})

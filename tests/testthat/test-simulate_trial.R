test_that("simulate_trial produces correct dimensions (primary)", {
  set.seed(42)
  n_int <- 50
  n_ext <- 30
  X_int <- data.frame(x1 = rnorm(n_int), x2 = rnorm(n_int))
  X_ext <- data.frame(x1 = rnorm(n_ext), x2 = rnorm(n_ext))

  specs <- list(
    list(
      effect = 0, model_form_x = c("1" = 1, "x1" = 0.5, "x2" = -0.3),
      noise_mean = 0, noise_sd = 1
    ),
    list(
      effect = 1, model_form_x = c("1" = 2, "x1" = 0.3, "x2" = -0.1),
      noise_mean = 0, noise_sd = 1
    )
  )

  result <- simulate_trial(X_int, X_ext,
    num_treated = 30, OLE_flag = FALSE,
    T_cross = 2, outcome_model_specs = specs
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), n_int + n_ext)
  expect_true(all(c("x1", "x2", "A", "S", "y1", "y2") %in% names(result)))
  expect_false("T_cross" %in% names(result))
})

test_that("simulate_trial produces correct dimensions (OLE)", {
  set.seed(42)
  n_int <- 50
  n_ext <- 30
  X_int <- data.frame(x1 = rnorm(n_int), x2 = rnorm(n_int))
  X_ext <- data.frame(x1 = rnorm(n_ext), x2 = rnorm(n_ext))

  specs <- list(
    list(
      effect = 0, model_form_x = c("1" = 1, "x1" = 0.5, "x2" = -0.3),
      noise_mean = 0, noise_sd = 1
    ),
    list(
      effect = 0, model_form_x = c("1" = 2, "x1" = 0.3, "x2" = -0.1),
      noise_mean = 0, noise_sd = 1
    ),
    list(
      effect = 1.5, model_form_x = c("1" = 2, "x1" = 0.3, "x2" = -0.1),
      noise_mean = 0, noise_sd = 1
    ),
    list(
      effect = 2.0, model_form_x = c("1" = 2, "x1" = 0.3, "x2" = -0.1),
      noise_mean = 0, noise_sd = 1
    )
  )

  result <- simulate_trial(X_int, X_ext,
    num_treated = 30, OLE_flag = TRUE,
    T_cross = 2, outcome_model_specs = specs
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), n_int + n_ext)
  expect_true(all(c("x1", "x2", "A", "S", "T_cross", "y1", "y2", "y3", "y4") %in% names(result)))
})

test_that("simulate_trial assigns correct treatment counts", {
  set.seed(42)
  n_int <- 100
  n_ext <- 50
  num_treated <- 60
  X_int <- data.frame(x1 = rnorm(n_int))
  X_ext <- data.frame(x1 = rnorm(n_ext))

  specs <- list(
    list(
      effect = 0, model_form_x = c("1" = 1, "x1" = 0.5),
      noise_mean = 0, noise_sd = 1
    )
  )

  result <- simulate_trial(X_int, X_ext,
    num_treated = num_treated, OLE_flag = FALSE,
    T_cross = 1, outcome_model_specs = specs
  )

  internal <- result[result$S == 1, ]
  external <- result[result$S == 0, ]

  expect_equal(sum(internal$A), num_treated)
  expect_equal(nrow(internal), n_int)
  expect_equal(nrow(external), n_ext)
  expect_true(all(external$A == 0))
})

test_that("simulate_trial S indicator is correct", {
  set.seed(42)
  n_int <- 40
  n_ext <- 20
  X_int <- data.frame(x1 = rnorm(n_int))
  X_ext <- data.frame(x1 = rnorm(n_ext))

  specs <- list(
    list(
      effect = 0, model_form_x = c("1" = 0, "x1" = 1),
      noise_mean = 0, noise_sd = 0.1
    )
  )

  result <- simulate_trial(X_int, X_ext,
    num_treated = 20, OLE_flag = FALSE,
    T_cross = 1, outcome_model_specs = specs
  )

  expect_equal(sum(result$S == 1), n_int)
  expect_equal(sum(result$S == 0), n_ext)
})

test_that("simulate_trial is reproducible with set.seed", {
  n_int <- 30
  n_ext <- 20
  X_int <- data.frame(x1 = rnorm(n_int), x2 = rnorm(n_int))
  X_ext <- data.frame(x1 = rnorm(n_ext), x2 = rnorm(n_ext))

  specs <- list(
    list(
      effect = 1, model_form_x = c("1" = 1, "x1" = 0.5, "x2" = -0.3),
      noise_mean = 0, noise_sd = 1
    )
  )

  set.seed(123)
  result1 <- simulate_trial(X_int, X_ext,
    num_treated = 15, OLE_flag = FALSE,
    T_cross = 1, outcome_model_specs = specs
  )

  set.seed(123)
  result2 <- simulate_trial(X_int, X_ext,
    num_treated = 15, OLE_flag = FALSE,
    T_cross = 1, outcome_model_specs = specs
  )

  expect_equal(result1, result2)
})

test_that("simulate_trial rownames are sequential", {
  set.seed(42)
  n_int <- 20
  n_ext <- 10
  X_int <- data.frame(x1 = rnorm(n_int))
  X_ext <- data.frame(x1 = rnorm(n_ext))

  specs <- list(
    list(
      effect = 0, model_form_x = c("1" = 1, "x1" = 0),
      noise_mean = 0, noise_sd = 1
    )
  )

  result <- simulate_trial(X_int, X_ext,
    num_treated = 10, OLE_flag = FALSE,
    T_cross = 1, outcome_model_specs = specs
  )

  expect_equal(rownames(result), as.character(1:(n_int + n_ext)))
})

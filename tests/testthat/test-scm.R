test_that(".scm_subject_sc returns convex weights and their predictions", {
  skip_if_not_installed("ECOSolveR")
  outs <- paste0("y", 1:4)
  vars <- c(paste0("x", 1:5), outs)
  ext <- SyntheticData[SyntheticData$S == 0, vars][1:15, ]
  ctrl <- SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 0, vars][1:3, ]
  X00 <- unname(t(as.matrix(rbind(ext, ctrl[1, ]))))
  X10 <- unname(t(as.matrix(ctrl)))
  rownames(X00) <- rownames(X10) <- vars

  fit <- suppressWarnings(
    .scm_subject_sc(2, X10, X00, c("y3", "y4"), lambda = 0.1)
  )
  w <- as.vector(fit[[1]])
  expect_all_true(w > -1e-6)
  expect_equal(sum(w), 1, tolerance = 1e-6)
  expect_equal(as.vector(fit[[2]]), as.vector(X00[c("y3", "y4"), ] %*% w))

  exact <- suppressWarnings(
    .scm_subject_sc(1, X10, X00, c("y3", "y4"), lambda = 0.1)
  )
  expect_equal(as.vector(exact[[1]])[16], 1, tolerance = 1e-4)
})

test_that(".scm_lambdacv searches a real lambda path", {
  skip_if_not_installed("ECOSolveR")
  outs <- paste0("y", 1:4)
  vars <- c(paste0("x", 1:5), outs)
  X00 <- unname(t(as.matrix(SyntheticData[SyntheticData$S == 0, vars][1:12, ])))
  rownames(X00) <- vars

  lambda <- suppressWarnings(.scm_lambdacv(
    X00, c("y3", "y4"),
    lambda_min = 0, lambda_max = 0.2, nlambda = 3
  ))

  expect_length(lambda, 1)
  expect_contains(c(0, 0.1, 0.2), lambda)
})

test_that("scm() runs end to end on a small data set", {
  skip_if_not_installed("ECOSolveR")
  d <- rbind(
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 1, ][1:10, ],
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 0, ][1:10, ],
    SyntheticData[SyntheticData$S == 0, ][1:15, ]
  )
  analysis <- setup_analysis_OLE(
    data = d,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    T_cross = 2,
    method_OLE_obj = scm(bootstrap = 5)
  )

  set.seed(1)
  res <- suppressWarnings(run_analysis(analysis))

  expect_identical(rownames(res), c("tau3", "tau4"))
  expect_all_true(is.finite(unlist(res)))
})

test_that("the scm bootstrap statistic equals the estimate on the original sample", {
  skip_if_not_installed("ECOSolveR")
  outs <- c("y1", "y2", "y3", "y4")
  covs <- c("x1", "x2", "x3", "x4", "x5")
  d <- rbind(
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 1, ][1:10, ],
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 0, ][1:10, ],
    SyntheticData[SyntheticData$S == 0, ][1:15, ]
  )
  method <- scm(lambda_min = 0.05, lambda_max = 0.05, nlambda = 1, bootstrap = 2)
  set.seed(1)
  res <- suppressWarnings(estimate(
    method,
    data = d, outcomes = outs, treatment = "A", trial_status = "S",
    covariates = covs, T_cross = 2
  ))
  df <- .build_analysis_df(d, outs, "A", "S", covs)
  boot_pe <- .scm_boot_statistic(
    df, seq_len(nrow(df)),
    outcomes = outs, covariates = covs, T_cross = 2, lambda = 0.05
  )
  expect_equal(unname(boot_pe), res$point_estimates)
})

test_that("scm() runs with two external controls and rejects one", {
  skip_if_not_installed("ECOSolveR")
  fit <- function(n_ext) {
    d <- rbind(
      SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 1, ][1:10, ],
      SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 0, ][1:10, ],
      SyntheticData[SyntheticData$S == 0, ][seq_len(n_ext), ]
    )
    analysis <- setup_analysis_OLE(
      d, "S", "A", c("y1", "y2", "y3", "y4"), c("x1", "x2", "x3", "x4", "x5"),
      scm(bootstrap = 2),
      T_cross = 2
    )
    set.seed(1)
    suppressWarnings(run_analysis(analysis))
  }
  expect_all_true(is.finite(fit(2)$point_estimates))
  expect_error(fit(1), "scm.*at least 2 external controls")
})

test_that("scm() names a covariate that is not numeric", {
  skip_if_not_installed("ECOSolveR")
  d <- rbind(
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 1, ][1:10, ],
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 0, ][1:10, ],
    SyntheticData[SyntheticData$S == 0, ][1:15, ]
  )
  d$x1 <- factor(d$x1, levels = c(0, 1), labels = c("type II", "type III"))
  analysis <- setup_analysis_OLE(
    d, "S", "A", c("y1", "y2", "y3", "y4"), c("x1", "x2", "x3", "x4", "x5"),
    scm(bootstrap = 2),
    T_cross = 2
  )
  expect_error(run_analysis(analysis), "scm.*numeric.*x1")
})

test_that("scm() rejects lambda settings it cannot use", {
  expect_error(scm(lambda_max = Inf), "lambda_max")
  expect_error(scm(lambda_min = Inf, lambda_max = Inf), "lambda_min")
  expect_error(
    scm(lambda_min = 0, lambda_max = 100, nlambda = 1),
    "nlambda = 1.*lambda_min"
  )
  expect_s4_class(
    scm(lambda_min = 0.05, lambda_max = 0.05, nlambda = 1),
    "scm_method"
  )
})

test_that("scm() reports the selected lambda", {
  skip_if_not_installed("ECOSolveR")
  d <- rbind(
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 1, ][1:10, ],
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 0, ][1:10, ],
    SyntheticData[SyntheticData$S == 0, ][1:15, ]
  )
  fit <- function(method) {
    analysis <- setup_analysis_OLE(
      d, "S", "A", c("y1", "y2", "y3", "y4"), c("x1", "x2", "x3", "x4", "x5"),
      method,
      T_cross = 2
    )
    set.seed(1)
    suppressWarnings(run_analysis(analysis))
  }
  fixed <- fit(scm(lambda_min = 0.05, lambda_max = 0.05, nlambda = 1, bootstrap = 2))
  expect_identical(attr(fixed, "lambda"), 0.05)
  searched <- fit(scm(lambda_min = 0, lambda_max = 0.1, nlambda = 2, bootstrap = 2))
  expect_in(attr(searched, "lambda"), c(0, 0.1))
})

test_that("scm() with a dominant penalty uses nearest-neighbour synthetic controls", {
  skip_if_not_installed("ECOSolveR")
  df <- make_ole_data(n_trt = 5, n_ctrl = 8, n_ext = 25, n_time = 4)
  method <- scm(lambda_min = 1e4, lambda_max = 1e4, nlambda = 1, bootstrap = 5)
  set.seed(1)
  res <- suppressWarnings(run_analysis(ole_analysis(df, method, T_cross = 2)))

  match_on <- c("x1", "x2", "y1", "y2")
  ctrl <- as.matrix(df[df$S == 1 & df$A == 0, match_on])
  ext <- df[df$S == 0, ]
  nn <- apply(ctrl, 1, \(x) which.min(colSums((t(ext[, match_on]) - x)^2)))
  expected <- colMeans(df[df$S == 1 & df$A == 1, c("y3", "y4")]) -
    colMeans(ext[nn, c("y3", "y4")])
  expect_equal(res$point_estimates, unname(expected), tolerance = 1e-5)
  expect_all_true(res$lower_CI_boot < res$upper_CI_boot)

  d <- .build_analysis_df(df, ole_outcomes(df), "A", "S", c("x1", "x2"))
  stat <- suppressWarnings(.scm_boot_statistic(
    d, seq_len(nrow(d)), ole_outcomes(df), c("x1", "x2"), 2,
    lambda = 1e4
  ))
  expect_equal(unname(stat), unname(expected), tolerance = 1e-5)
})

test_that("scm() confidence intervals honour alpha", {
  skip_on_cran()
  skip_if_not_installed("ECOSolveR")
  df <- make_ole_data(n_trt = 6, n_ctrl = 6, n_ext = 15, n_time = 4)
  method <- scm(lambda_min = 0.01, lambda_max = 0.01, nlambda = 1, bootstrap = 40)
  set.seed(3)
  wide <- suppressWarnings(run_analysis(ole_analysis(df, method, 2, alpha = 0.05)))
  set.seed(3)
  narrow <- suppressWarnings(run_analysis(ole_analysis(df, method, 2, alpha = 0.5)))
  expect_all_true(narrow$lower_CI_boot > wide$lower_CI_boot)
  expect_all_true(narrow$upper_CI_boot < wide$upper_CI_boot)
})

test_that(".scm_lambdacv picks the lambda with the smallest held-out error", {
  skip_if_not_installed("ECOSolveR")
  vars <- c(paste0("x", 1:5), paste0("y", 1:4))
  X00 <- unname(t(as.matrix(SyntheticData[SyntheticData$S == 0, vars][1:12, ])))
  rownames(X00) <- vars
  ole <- c("y3", "y4")
  grid <- seq(0, 0.1, length.out = 5)
  loocv_mse <- function(lambda) {
    pred <- vapply(seq_len(ncol(X00)), \(j) {
      fit <- .scm_subject_sc(1, X00[, j, drop = FALSE], X00[, -j], ole, lambda)
      as.vector(fit[[2]])
    }, numeric(2))
    mean((X00[ole, ] - pred)^2)
  }
  mse <- suppressWarnings(vapply(grid, loocv_mse, numeric(1)))

  lambda <- suppressWarnings(.scm_lambdacv(X00, ole, 0, 0.1, nlambda = 5))
  expect_equal(lambda, grid[which.min(mse)])
  expect_gt(max(mse) - min(mse), 1)
})

test_that("scm() stops with a clear error when the solver fails", {
  skip_if_not_installed("ECOSolveR")
  d <- rbind(
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 1, ][1:10, ],
    SyntheticData[SyntheticData$S == 1 & SyntheticData$A == 0, ][1:10, ],
    SyntheticData[SyntheticData$S == 0, ][1:15, ]
  )
  fit <- function() {
    analysis <- setup_analysis_OLE(
      d, "S", "A", c("y1", "y2", "y3", "y4"), c("x1", "x2", "x3", "x4", "x5"),
      scm(bootstrap = 2),
      T_cross = 2
    )
    set.seed(1)
    suppressWarnings(run_analysis(analysis))
  }
  local_mocked_bindings(.scm_status = function(prob) "infeasible")
  expect_error(fit(), "solver status is 'infeasible'.*rescale")
  local_mocked_bindings(.scm_status = function(prob) "optimal_inaccurate")
  expect_all_true(is.finite(fit()$point_estimates))
})

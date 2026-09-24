tol <- 1e-6

test_that("DID-EC-IPW point estimates and bootstrap CIs", {
  method <- did_ec_ipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
    bootstrap = 50
  )
  analysis <- setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    T_cross = 2,
    method_OLE_obj = method
  )

  set.seed(42)
  res <- run_analysis(analysis)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$point_estimates[1], 2.0759256334, tolerance = tol)
  expect_equal(res$point_estimates[2], 4.3894380397, tolerance = tol)
  expect_equal(res$lower_CI_boot[1], -0.1985438132, tolerance = tol)
  expect_equal(res$lower_CI_boot[2], 1.7688904174, tolerance = tol)
  expect_equal(res$upper_CI_boot[1], 4.0048311340, tolerance = tol)
  expect_equal(res$upper_CI_boot[2], 8.2132873140, tolerance = tol)
})

test_that("DID-EC-IPW marginal treatment model (default trt_formula)", {
  method <- did_ec_ipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    bootstrap = 50
  )
  analysis <- setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    T_cross = 2,
    method_OLE_obj = method
  )

  set.seed(42)
  res <- run_analysis(analysis)

  expect_equal(res$point_estimates[1], 2.3233521560, tolerance = tol)
  expect_equal(res$point_estimates[2], 4.6325195543, tolerance = tol)
  expect_equal(res$lower_CI_boot[1], 0.1003513361, tolerance = tol)
  expect_equal(res$upper_CI_boot[2], 8.2159408390, tolerance = tol)
})

test_that("DID-EC-IPW marginal model matches an intercept-only treatment model", {
  covs <- c("x1", "x2", "x3", "x4", "x5")
  outcomes <- c("y1", "y2", "y3", "y4")
  df <- .build_analysis_df(SyntheticData, outcomes, "A", "S", covs)
  Y <- as.matrix(df[, outcomes, drop = FALSE])
  ps <- "S ~ x1 + x2 + x3 + x4 + x5"

  marginal <- .did_ec_ipw_core(df, Y, df$S, df$A, 2, ps, NULL)$tau
  intercept <- .did_ec_ipw_core(df, Y, df$S, df$A, 2, ps, "A ~ 1")$tau

  expect_equal(marginal, intercept, tolerance = 0)
})

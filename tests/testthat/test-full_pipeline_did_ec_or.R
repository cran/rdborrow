tol <- 1e-6

test_that("DID-EC-OR point estimates and bootstrap CIs", {
  model_forms <- c(
    "y1 ~ x1 + x2 + x3 + x4 + x5",
    "y2 ~ x1 + x2 + x3 + x4 + x5",
    "y3 ~ x1 + x2 + x3 + x4 + x5",
    "y4 ~ x1 + x2 + x3 + x4 + x5"
  )
  method <- did_ec_or(
    outcome_formula_ext = model_forms,
    outcome_formula_rct_ctrl = model_forms,
    outcome_formula_rct_trt = model_forms,
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
  res <- suppressWarnings(run_analysis(analysis))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$point_estimates[1], 1.5689465845, tolerance = tol)
  expect_equal(res$point_estimates[2], 4.4078336543, tolerance = tol)
  expect_equal(res$lower_CI_boot[1], -0.5122539446, tolerance = tol)
  expect_equal(res$lower_CI_boot[2], 2.7815366451, tolerance = tol)
  expect_equal(res$upper_CI_boot[1], 3.1261896870, tolerance = tol)
  expect_equal(res$upper_CI_boot[2], 7.9430452410, tolerance = tol)
})

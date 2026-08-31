tol <- 1e-6

test_that("SCM point estimates and bootstrap CIs", {
  skip_on_cran()

  method <- scm(
    lambda_min = 0.0005,
    lambda_max = 0.0005,
    nlambda = 1,
    bootstrap = 50,
    bootstrap_ci_type = "perc"
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
  expect_equal(res$point_estimates[1], 2.1383459186, tolerance = tol)
  expect_equal(res$point_estimates[2], 3.8894184460, tolerance = tol)
  expect_all_true(res$lower_CI_boot <= res$point_estimates)
  expect_all_true(res$upper_CI_boot >= res$point_estimates)
})

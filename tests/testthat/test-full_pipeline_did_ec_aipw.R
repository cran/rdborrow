tol <- 1e-6

test_that("DID-EC-AIPW point estimates and bootstrap CIs", {
  method <- did_ec_aipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
    outcome_formula = c(
      "y1 ~ x1 + x2 + x3 + x4 + x5",
      "y2 ~ x1 + x2 + x3 + x4 + x5",
      "y3 ~ x1 + x2 + x3 + x4 + x5",
      "y4 ~ x1 + x2 + x3 + x4 + x5"
    ),
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
  expect_equal(res$point_estimates[1], 2.0417274627, tolerance = tol)
  expect_equal(res$point_estimates[2], 4.0361177594, tolerance = tol)
  expect_equal(res$lower_CI_boot[1], -0.2867417425, tolerance = tol)
  expect_equal(res$lower_CI_boot[2], 1.8145638636, tolerance = tol)
  expect_equal(res$upper_CI_boot[1], 4.0728301930, tolerance = tol)
  expect_equal(res$upper_CI_boot[2], 8.6316113430, tolerance = tol)
})

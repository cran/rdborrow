test_that("run_analysis dispatches ec_ipw correctly", {
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
  analysis <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method
  )
  res <- run_analysis(analysis)

  expect_type(res, "list")
  expect_named(res, c("results", "borrow_weight"))
  expect_s3_class(res$results, "data.frame")
  expect_equal(nrow(res$results), 2)
  expect_true(all(c("point_estimates", "standard_deviation") %in% names(res$results)))
})

test_that("run_analysis dispatches ec_aipw correctly", {
  method <- ec_aipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    outcome_formula = c("y1 ~ x1 + x2 + x3 + x4 + x5", "y2 ~ x1 + x2 + x3 + x4 + x5")
  )
  analysis <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method
  )
  res <- run_analysis(analysis)

  expect_type(res, "list")
  expect_named(res, c("results", "borrow_weight"))
  expect_equal(nrow(res$results), 2)
})

test_that("run_analysis dispatches did_ec_ipw correctly", {
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
    method_OLE_obj = method,
    T_cross = 2
  )
  res <- run_analysis(analysis)

  expect_s3_class(res, "data.frame")
  expect_true("point_estimates" %in% names(res))
  expect_equal(nrow(res), 2)
})

test_that("run_analysis dispatches did_ec_aipw correctly", {
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
    method_OLE_obj = method,
    T_cross = 2
  )
  res <- run_analysis(analysis)

  expect_s3_class(res, "data.frame")
  expect_true("point_estimates" %in% names(res))
})

test_that("run_analysis dispatches did_ec_or correctly", {
  method <- did_ec_or(
    outcome_formula_ext = c(
      "y1 ~ x1 + x2 + x3 + x4 + x5",
      "y2 ~ x1 + x2 + x3 + x4 + x5",
      "y3 ~ x1 + x2 + x3 + x4 + x5",
      "y4 ~ x1 + x2 + x3 + x4 + x5"
    ),
    outcome_formula_rct_ctrl = c(
      "y1 ~ x1 + x2 + x3 + x4 + x5",
      "y2 ~ x1 + x2 + x3 + x4 + x5",
      "y3 ~ x1 + x2 + x3 + x4 + x5",
      "y4 ~ x1 + x2 + x3 + x4 + x5"
    ),
    outcome_formula_rct_trt = c(
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
    method_OLE_obj = method,
    T_cross = 2
  )
  res <- suppressWarnings(run_analysis(analysis))

  expect_s3_class(res, "data.frame")
  expect_true("point_estimates" %in% names(res))
})

test_that("run_analysis dispatches scm correctly", {
  skip_on_cran()
  skip_if_not_installed("ECOSolveR")

  method <- scm(bootstrap = 50)
  analysis <- setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_OLE_obj = method,
    T_cross = 2
  )
  res <- run_analysis(analysis)

  expect_s3_class(res, "data.frame")
  expect_true("point_estimates" %in% names(res))
})

test_that("run_analysis quiet argument suppresses output", {
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
  analysis <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method
  )

  expect_silent(run_analysis(analysis, quiet = TRUE))
})

test_that("run_analysis works with single outcome", {
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
  analysis <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method
  )
  res <- run_analysis(analysis)

  expect_equal(nrow(res$results), 1)
})

test_that("run_analysis respects alpha parameter", {
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
  analysis_wide <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method,
    alpha = 0.01
  )
  analysis_narrow <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method,
    alpha = 0.10
  )
  res_wide <- run_analysis(analysis_wide)
  res_narrow <- run_analysis(analysis_narrow)

  ci_width_wide <- res_wide$results$upper_CI_normal - res_wide$results$lower_CI_normal
  ci_width_narrow <- res_narrow$results$upper_CI_normal - res_narrow$results$lower_CI_normal
  expect_true(all(ci_width_wide > ci_width_narrow))
})

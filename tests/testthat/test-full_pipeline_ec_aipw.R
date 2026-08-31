tol <- 1e-6

test_that("EC-AIPW optimal weight", {
  method <- ec_aipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    outcome_formula = c(
      "y1 ~ x1 + x2 + x3 + x4 + x5",
      "y2 ~ x1 + x2 + x3 + x4 + x5"
    )
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

  expect_equal(res$borrow_weight, 0.1475196487, tolerance = tol)
  expect_equal(res$results$point_estimates[1], -0.5463256250, tolerance = tol)
  expect_equal(res$results$point_estimates[2], 0.5401749583, tolerance = tol)
  expect_equal(res$results$standard_deviation[1], 0.5305614087, tolerance = tol)
  expect_equal(res$results$standard_deviation[2], 0.5543275065, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[1], -1.5862068777, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[2], -0.5462869900, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[1], 0.4935556277, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[2], 1.6266369066, tolerance = tol)
})

test_that("EC-AIPW zero weight (no borrowing)", {
  method <- ec_aipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    outcome_formula = c(
      "y1 ~ x1 + x2 + x3 + x4 + x5",
      "y2 ~ x1 + x2 + x3 + x4 + x5"
    ),
    weight = 0
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

  expect_equal(res$borrow_weight, 0)
  expect_equal(res$results$point_estimates[1], -0.4361151144, tolerance = tol)
  expect_equal(res$results$point_estimates[2], 0.4422248202, tolerance = tol)
  expect_equal(res$results$standard_deviation[1], 0.5552064946, tolerance = tol)
  expect_equal(res$results$standard_deviation[2], 0.5701633244, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[1], -1.5242998477, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[2], -0.6752747610, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[1], 0.6520696190, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[2], 1.5597244013, tolerance = tol)
})

test_that("EC-AIPW fixed weight 0.3", {
  method <- ec_aipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    outcome_formula = c(
      "y1 ~ x1 + x2 + x3 + x4 + x5",
      "y2 ~ x1 + x2 + x3 + x4 + x5"
    ),
    weight = 0.3
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

  expect_equal(res$borrow_weight, 0.3)
  expect_equal(res$results$point_estimates[1], -0.6602422289, tolerance = tol)
  expect_equal(res$results$point_estimates[2], 0.6414189052, tolerance = tol)
  expect_equal(res$results$standard_deviation[1], 0.5263737407, tolerance = tol)
  expect_equal(res$results$standard_deviation[2], 0.5832105340, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[1], -1.6919158032, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[2], -0.5016527368, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[1], 0.3714313453, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[2], 1.7844905472, tolerance = tol)
})

test_that("EC-AIPW with bootstrap", {
  method <- ec_aipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    outcome_formula = c(
      "y1 ~ x1 + x2 + x3 + x4 + x5",
      "y2 ~ x1 + x2 + x3 + x4 + x5"
    ),
    bootstrap = 50,
    bootstrap_ci_type = "perc"
  )
  analysis <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method
  )

  set.seed(42)
  res <- run_analysis(analysis)

  expect_equal(res$results$point_estimates[1], -0.5463256250, tolerance = tol)
  expect_equal(res$results$point_estimates[2], 0.5401749583, tolerance = tol)
  expect_equal(res$results$standard_deviation[1], 0.5080198258, tolerance = tol)
  expect_equal(res$results$standard_deviation[2], 0.5768928272, tolerance = tol)
  expect_equal(res$results$lower_CI_boot[1], -1.7599337570, tolerance = tol)
  expect_equal(res$results$lower_CI_boot[2], -0.5941611042, tolerance = tol)
  expect_equal(res$results$upper_CI_boot[1], 0.5020300888, tolerance = tol)
  expect_equal(res$results$upper_CI_boot[2], 1.7156835698, tolerance = tol)
  expect_equal(res$borrow_weight, 0.1475196487, tolerance = tol)
  expect_named(
    res$results,
    c("point_estimates", "standard_deviation", "lower_CI_boot", "upper_CI_boot")
  )
})

test_that("EC-AIPW errors when outcome_formula length mismatches outcomes", {
  method <- ec_aipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    outcome_formula = c("y1 ~ x1 + x2 + x3 + x4 + x5")
  )
  analysis <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = method
  )
  expect_error(run_analysis(analysis), "one formula per outcome")
})

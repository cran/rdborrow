tol <- 1e-6

test_that("EC-IPW optimal weight", {
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

  expect_equal(res$borrow_weight, 0.1475196487, tolerance = tol)
  expect_equal(res$results$point_estimates[1], -0.1971968926, tolerance = tol)
  expect_equal(res$results$point_estimates[2], 0.4697208867, tolerance = tol)
  expect_equal(res$results$standard_deviation[1], 0.5134017502, tolerance = tol)
  expect_equal(res$results$standard_deviation[2], 0.5410007056, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[1], -1.2034458325, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[2], -0.5906210120, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[1], 0.8090520473, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[2], 1.5300627854, tolerance = tol)
})

test_that("EC-IPW zero weight (no borrowing)", {
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5", weight = 0)
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
  expect_equal(res$borrow_weight, 0)
  expect_equal(res$results$point_estimates[1], -0.0280870419, tolerance = tol)
  expect_equal(res$results$point_estimates[2], 0.4095955812, tolerance = tol)
  expect_equal(res$results$standard_deviation[1], 0.5367986594, tolerance = tol)
  expect_equal(res$results$standard_deviation[2], 0.5625763260, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[1], -1.0801930814, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[2], -0.6930337563, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[1], 1.0240189975, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[2], 1.5122249187, tolerance = tol)
})

test_that("EC-IPW fixed weight 0.3", {
  method <- ec_ipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
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
  expect_equal(res$results$point_estimates[1], -0.3719934684, tolerance = tol)
  expect_equal(res$results$point_estimates[2], 0.5318680501, tolerance = tol)
  expect_equal(res$results$standard_deviation[1], 0.5148090221, tolerance = tol)
  expect_equal(res$results$standard_deviation[2], 0.5657008041, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[1], -1.3810006106, tolerance = tol)
  expect_equal(res$results$lower_CI_normal[2], -0.5768851520, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[1], 0.6370136739, tolerance = tol)
  expect_equal(res$results$upper_CI_normal[2], 1.6406212522, tolerance = tol)
})

test_that("EC-IPW with bootstrap", {
  method <- ec_ipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
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

  expect_equal(res$results$point_estimates[1], -0.1971968926, tolerance = tol)
  expect_equal(res$results$point_estimates[2], 0.4697208867, tolerance = tol)
  expect_equal(res$results$standard_deviation[1], 0.5189004543, tolerance = tol)
  expect_equal(res$results$standard_deviation[2], 0.5423807156, tolerance = tol)
  expect_equal(res$results$lower_CI_boot[1], -1.4650566979, tolerance = tol)
  expect_equal(res$results$lower_CI_boot[2], -0.6567154973, tolerance = tol)
  expect_equal(res$results$upper_CI_boot[1], 0.9805890544, tolerance = tol)
  expect_equal(res$results$upper_CI_boot[2], 1.5445094270, tolerance = tol)
  expect_equal(res$borrow_weight, 0.1475196487, tolerance = tol)
  expect_named(
    res$results,
    c("point_estimates", "standard_deviation", "lower_CI_boot", "upper_CI_boot")
  )
})

test_that("setup_method_weighting is deprecated", {
  expect_error(setup_method_weighting(), "no longer functional")
})

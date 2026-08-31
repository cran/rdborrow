test_that("setup_analysis_primary returns valid object", {
  obj <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
  )
  expect_s4_class(obj, "analysis_primary_obj")
  expect_identical(obj@method_obj@method_name, "EC-IPW")
  expect_identical(obj@alpha, 0.05)
})

test_that("setup_analysis_primary inherits base validation", {
  method <- ec_ipw(ps_formula = "S ~ x1")
  expect_error(setup_analysis_primary(
    data = list(),
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_weighting_obj = method
  ))
  expect_error(setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "not_a_col",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_weighting_obj = method
  ))
})

test_that("setup_analysis_primary validates method type", {
  expect_error(setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_weighting_obj = did_ec_ipw(ps_formula = "S ~ x1", bootstrap = 50)
  ))
})

test_that("show method prints without error", {
  obj <- setup_analysis_primary(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_weighting_obj = ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
  )
  expect_output(show(obj), "analysis_primary_obj")
  expect_output(show(obj), "EC-IPW")
})

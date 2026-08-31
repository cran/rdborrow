test_that("setup_analysis_OLE returns valid object", {
  method <- did_ec_ipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
    bootstrap = 50
  )
  obj <- setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_OLE_obj = method,
    T_cross = 2
  )
  expect_s4_class(obj, "analysis_OLE_obj")
  expect_identical(obj@T_cross, 2)
  expect_identical(obj@alpha, 0.05)
})

test_that("setup_analysis_OLE inherits base validation", {
  method <- did_ec_ipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    bootstrap = 50
  )
  expect_error(setup_analysis_OLE(
    data = list(),
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_OLE_obj = method,
    T_cross = 2
  ))
  expect_error(setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "not_a_col",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_OLE_obj = method,
    T_cross = 2
  ))
})

test_that("setup_analysis_OLE validates method type", {
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
  expect_error(setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_OLE_obj = method,
    T_cross = 2
  ))
})

test_that("setup_analysis_OLE validates T_cross", {
  method <- did_ec_ipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    bootstrap = 50
  )
  expect_error(setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_OLE_obj = method,
    T_cross = -1
  ))
  expect_error(setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_OLE_obj = method,
    T_cross = 2
  ), "T_cross must be less than")
})

test_that("show method prints without error", {
  method <- did_ec_ipw(
    ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
    bootstrap = 50
  )
  obj <- setup_analysis_OLE(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_OLE_obj = method,
    T_cross = 2
  )
  expect_output(show(obj), "analysis_OLE_obj")
  expect_output(show(obj), "T_cross: 2")
})

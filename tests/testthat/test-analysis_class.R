test_that("setup_analysis returns valid object", {
  obj <- setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_obj = setup_method(method_name = "AIPW")
  )
  expect_s4_class(obj, "analysis_obj")
  expect_identical(obj@trial_status_col_name, "S")
  expect_identical(obj@treatment_col_name, "A")
  expect_identical(obj@outcome_col_name, c("y1", "y2"))
  expect_identical(obj@covariates_col_name, c("x1", "x2", "x3", "x4", "x5"))
  expect_identical(obj@alpha, 0.05)
})

test_that("setup_analysis validates data", {
  expect_error(setup_analysis(
    data = list(),
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj = setup_method()
  ))
})

test_that("setup_analysis validates column names exist in data", {
  expect_error(setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "not_a_col",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj = setup_method()
  ))
  expect_error(setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "not_a_col",
    covariates_col_name = "x1",
    method_obj = setup_method()
  ))
})

test_that("setup_analysis validates method_obj", {
  expect_error(setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj = list()
  ))
})

test_that("setup_analysis validates alpha", {
  expect_error(setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj = setup_method(),
    alpha = -0.1
  ))
  expect_error(setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj = setup_method(),
    alpha = 1.1
  ))
})

test_that("show method prints without error", {
  obj <- setup_analysis(
    data = SyntheticData,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_obj = setup_method(method_name = "AIPW")
  )
  expect_output(show(obj), "analysis_obj")
  expect_output(show(obj), "300")
})

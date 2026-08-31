test_that("setup_simulation returns valid object", {
  obj <- setup_simulation(
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_obj_list = list(setup_method(method_name = "AIPW")),
    method_description = "AIPW"
  )
  expect_s4_class(obj, "simulation_obj")
  expect_identical(obj@trial_status_col_name, "S")
  expect_identical(obj@method_description, "AIPW")
  expect_identical(obj@alpha, 0.05)
})

test_that("setup_simulation validates method_obj_list", {
  expect_error(setup_simulation(
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj_list = list(),
    method_description = character(0)
  ))
  expect_error(setup_simulation(
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj_list = list("not_a_method"),
    method_description = "bad"
  ))
})

test_that("setup_simulation validates method_description length", {
  expect_error(setup_simulation(
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj_list = list(setup_method()),
    method_description = c("a", "b")
  ))
})

test_that("setup_simulation validates alpha", {
  expect_error(setup_simulation(
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj_list = list(setup_method()),
    method_description = "m",
    alpha = 1.5
  ))
})

test_that("show method prints without error", {
  obj <- setup_simulation(
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_obj_list = list(setup_method(method_name = "AIPW")),
    method_description = "AIPW"
  )
  expect_output(show(obj), "simulation_obj")
  expect_output(show(obj), "AIPW")
})

test_that("setup_simulation_primary returns valid object", {
  obj <- setup_simulation_primary(
    data_matrix_list_null = list(SyntheticData, SyntheticData),
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_obj_list = list(setup_method(method_name = "AIPW")),
    true_effect = c(0, 0),
    method_description = "AIPW"
  )
  expect_s4_class(obj, "simulation_primary_obj")
  expect_length(obj@data_matrix_list_null, 2)
  expect_identical(obj@true_effect, c(0, 0))
  expect_identical(obj@data_matrix_list_alt, list())
  expect_identical(obj@alt_effect, numeric(0))
})

test_that("setup_simulation_primary validates data_matrix_list_null", {
  expect_error(setup_simulation_primary(
    data_matrix_list_null = list(),
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj_list = list(setup_method()),
    true_effect = 0,
    method_description = "m"
  ))
  expect_error(setup_simulation_primary(
    data_matrix_list_null = list("not_a_df"),
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj_list = list(setup_method()),
    true_effect = 0,
    method_description = "m"
  ))
})

test_that("setup_simulation_OLE returns valid object", {
  obj <- setup_simulation_OLE(
    data_matrix_list = list(SyntheticData),
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_obj_list = list(setup_method(method_name = "AIPW")),
    T_cross = 2,
    true_effect = c(0, 0),
    method_description = "AIPW"
  )
  expect_s4_class(obj, "simulation_OLE_obj")
  expect_identical(obj@T_cross, 2)
  expect_identical(obj@true_effect, c(0, 0))
})

test_that("setup_simulation_OLE validates T_cross", {
  expect_error(setup_simulation_OLE(
    data_matrix_list = list(SyntheticData),
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = "y1",
    covariates_col_name = "x1",
    method_obj_list = list(setup_method()),
    T_cross = -1,
    true_effect = 0,
    method_description = "m"
  ))
})

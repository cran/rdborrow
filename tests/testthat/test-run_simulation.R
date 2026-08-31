# helper: build minimal simulation datasets----
make_primary_sim_data <- function(ntrial = 3, seed = 99) {
  set.seed(seed)
  data_list <- lapply(seq_len(ntrial), function(i) {
    n <- 80
    df <- data.frame(
      x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n),
      A = c(rep(1, 40), rep(0, 40)),
      S = c(rep(1, 50), rep(0, 30))
    )
    df$y1 <- 1 + 0.5 * df$x1 - 0.3 * df$x2 + rnorm(n)
    df$y2 <- 2 + 0.3 * df$x1 + 0.2 * df$x3 + rnorm(n)
    df
  })
  data_list
}

make_OLE_sim_data <- function(ntrial = 3, seed = 99) {
  set.seed(seed)
  data_list <- lapply(seq_len(ntrial), function(i) {
    n <- 80
    df <- data.frame(
      x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n),
      A = c(rep(1, 40), rep(0, 40)),
      S = c(rep(1, 50), rep(0, 30))
    )
    df$y1 <- 1 + 0.5 * df$x1 + rnorm(n)
    df$y2 <- 2 + 0.3 * df$x1 + rnorm(n)
    df$y3 <- 3 + 0.2 * df$x1 + rnorm(n)
    df$y4 <- 4 + 0.1 * df$x1 + rnorm(n)
    df
  })
  data_list
}

test_that("run_simulation returns simulation_report_obj for primary", {
  data_null <- make_primary_sim_data()
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3")

  sim_obj <- setup_simulation_primary(
    data_matrix_list_null = data_null,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3"),
    method_obj_list = list(method),
    true_effect = 0,
    method_description = "IPW"
  )

  report <- run_simulation(sim_obj, quiet = TRUE)

  expect_s4_class(report, "simulation_report_obj")
  expect_equal(report@method_description, "IPW")
  expect_length(report@bias, 1)
  expect_length(report@variance, 1)
  expect_length(report@mse, 1)
  expect_length(report@coverage, 1)
  expect_length(report@type_I_error, 1)
})

test_that("run_simulation computes power when alt data provided", {
  data_null <- make_primary_sim_data(seed = 100)
  data_alt <- make_primary_sim_data(seed = 200)
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3")

  sim_obj <- setup_simulation_primary(
    data_matrix_list_null = data_null,
    data_matrix_list_alt = data_alt,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3"),
    method_obj_list = list(method),
    true_effect = 0,
    alt_effect = 2.0,
    method_description = "IPW"
  )

  report <- run_simulation(sim_obj, quiet = TRUE)

  expect_s4_class(report, "simulation_report_obj")
  expect_length(report@power, 1)
})

test_that("run_simulation handles multiple methods", {
  data_null <- make_primary_sim_data()
  method1 <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3")
  method2 <- ec_aipw(
    ps_formula = "S ~ x1 + x2 + x3",
    outcome_formula = c("y1 ~ x1 + x2 + x3", "y2 ~ x1 + x2 + x3")
  )

  sim_obj <- setup_simulation_primary(
    data_matrix_list_null = data_null,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3"),
    method_obj_list = list(method1, method2),
    true_effect = 0,
    method_description = c("IPW", "AIPW")
  )

  report <- run_simulation(sim_obj, quiet = TRUE)

  expect_length(report@bias, 2)
  expect_length(report@variance, 2)
  expect_equal(report@method_description, c("IPW", "AIPW"))
})

test_that("run_simulation works for OLE", {
  data_list <- make_OLE_sim_data()
  method <- did_ec_ipw(
    ps_formula = "S ~ x1 + x2 + x3",
    trt_formula = "A ~ x1 + x2 + x3",
    bootstrap = 50
  )

  sim_obj <- setup_simulation_OLE(
    data_matrix_list = data_list,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2", "y3", "y4"),
    covariates_col_name = c("x1", "x2", "x3"),
    method_obj_list = list(method),
    T_cross = 2,
    true_effect = 0,
    method_description = "DID-IPW"
  )

  report <- run_simulation(sim_obj, quiet = TRUE)

  expect_s4_class(report, "simulation_report_obj")
  expect_equal(report@method_description, "DID-IPW")
  expect_length(report@bias, 1)
  expect_length(report@type_I_error, 1)
})

test_that("run_simulation quiet=FALSE produces output", {
  data_null <- make_primary_sim_data(ntrial = 1)
  method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3")

  sim_obj <- setup_simulation_primary(
    data_matrix_list_null = data_null,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3"),
    method_obj_list = list(method),
    true_effect = 0,
    method_description = "IPW"
  )

  expect_output(run_simulation(sim_obj, quiet = FALSE), "Null:")
})

test_that("run_simulation errors on invalid object", {
  expect_error(run_simulation("not a simulation object"))
})

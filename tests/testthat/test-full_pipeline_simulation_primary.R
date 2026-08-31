# Regression tests for the Primary Simulation Workflow vignette.
# Reproduces the exact simulation pipeline from set.seed(2023) and verifies
# that the simulation_report_obj slot values remain unchanged.
#
# Reference: vignettes/primary_simulation_workflow.Rmd

# ---------- helper: generate vignette simulation data -----------------------
generate_primary_sim_data <- function() {
  set.seed(2023)

  data_matrix_list_null <- list()
  data_matrix_list_alt <- list()
  ntrial <- 3
  true_effect <- 0
  alt_effect <- 2.0

  for (trial_iter in 1:ntrial) {
    normal <- copula::normalCopula(param = c(0.8), dim = 4, dispstr = "ar1")

    X_int <- simulate_X_copula(
      n = 200, p = 4, cp = normal,
      margins = c("binom", "binom", "binom", "exp"),
      paramMargins = list(
        list(size = 1, prob = 0.7), list(size = 1, prob = 0.9),
        list(size = 1, prob = 0.3), list(rate = 1 / 10)
      )
    )
    X_int$x4 <- round(X_int$x4) + 1
    X_int$x5 <- 30 + 10 * X_int$x1 + 7 * X_int$x2 - 6 * X_int$x3 -
      0.5 * X_int$x4 + rnorm(200, 0, 10)

    X_ext <- simulate_X_copula(
      n = 100, p = 4, cp = normal,
      margins = c("binom", "binom", "binom", "exp"),
      paramMargins = list(
        list(size = 1, prob = 0.7), list(size = 1, prob = 0.9),
        list(size = 1, prob = 0.3), list(rate = 1 / 10)
      )
    )
    X_ext$x4 <- round(X_ext$x4) + 1
    X_ext$x5 <- 50 + 10 * X_ext$x1 + 2 * X_ext$x2 - 1 * X_ext$x3 -
      0.3 * X_ext$x4 + rnorm(100, 0, 10)

    varnames <- c("1", paste0("x", 1:5))
    model_form_x_t1 <- setNames(c(10.0, 0.05, -1.5, -1.0, -0.2, -0.1), varnames)
    model_form_x_t2 <- setNames(c(6.0, 0.5, -0.5, -1.0, -0.3, -0.06), varnames)

    outcome_model_specs <- list(
      list(
        effect = 0, model_form_x = model_form_x_t1,
        noise_mean = 0, noise_sd = 4
      ),
      list(
        effect = true_effect, model_form_x = model_form_x_t2,
        noise_mean = 0, noise_sd = 4
      )
    )

    data_matrix_list_null[[trial_iter]] <- simulate_trial(
      X_int, X_ext,
      num_treated = 100, OLE_flag = FALSE,
      T_cross = 2, outcome_model_specs
    )
  }

  for (trial_iter in 1:ntrial) {
    normal <- copula::normalCopula(param = c(0.8), dim = 4, dispstr = "ar1")

    X_int <- simulate_X_copula(
      n = 200, p = 4, cp = normal,
      margins = c("binom", "binom", "binom", "exp"),
      paramMargins = list(
        list(size = 1, prob = 0.7), list(size = 1, prob = 0.9),
        list(size = 1, prob = 0.3), list(rate = 1 / 10)
      )
    )
    X_int$x4 <- round(X_int$x4) + 1
    X_int$x5 <- 30 + 10 * X_int$x1 + 7 * X_int$x2 - 6 * X_int$x3 -
      0.5 * X_int$x4 + rnorm(200, 0, 10)

    X_ext <- simulate_X_copula(
      n = 100, p = 4, cp = normal,
      margins = c("binom", "binom", "binom", "exp"),
      paramMargins = list(
        list(size = 1, prob = 0.7), list(size = 1, prob = 0.9),
        list(size = 1, prob = 0.3), list(rate = 1 / 10)
      )
    )
    X_ext$x4 <- round(X_ext$x4) + 1
    X_ext$x5 <- 50 + 10 * X_ext$x1 + 2 * X_ext$x2 - 1 * X_ext$x3 -
      0.3 * X_ext$x4 + rnorm(100, 0, 10)

    varnames <- c("1", paste0("x", 1:5))
    model_form_x_t1 <- setNames(c(10.0, 0.05, -1.5, -1.0, -0.2, -0.1), varnames)
    model_form_x_t2 <- setNames(c(6.0, 0.5, -0.5, -1.0, -0.3, -0.06), varnames)

    outcome_model_specs <- list(
      list(
        effect = 0, model_form_x = model_form_x_t1,
        noise_mean = 0, noise_sd = 4
      ),
      list(
        effect = alt_effect, model_form_x = model_form_x_t2,
        noise_mean = 0, noise_sd = 4
      )
    )

    data_matrix_list_alt[[trial_iter]] <- simulate_trial(
      X_int, X_ext,
      num_treated = 100, OLE_flag = FALSE,
      T_cross = 2, outcome_model_specs
    )
  }

  list(null = data_matrix_list_null, alt = data_matrix_list_alt)
}

# =============================================================================
# Parametric inference simulation
# =============================================================================
test_that("Primary simulation (parametric) report matches vignette", {
  sim_data <- generate_primary_sim_data()

  method_obj_list <- list(
    ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5"),
    ec_aipw(
      ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
      outcome_formula = c(
        "y1 ~ x1 + x2 + x3 + x4 + x5",
        "y2 ~ x1 + x2 + x3 + x4 + x5"
      )
    ),
    ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5", weight = 0),
    ec_aipw(
      ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
      outcome_formula = c(
        "y1 ~ x1 + x2 + x3 + x4 + x5",
        "y2 ~ x1 + x2 + x3 + x4 + x5"
      ),
      weight = 0
    )
  )

  sim_obj <- setup_simulation_primary(
    data_matrix_list_null = sim_data$null,
    data_matrix_list_alt = sim_data$alt,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = c("y1", "y2"),
    covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
    method_obj_list = method_obj_list,
    true_effect = 0,
    alt_effect = 2.0,
    alpha = 0.05,
    method_description = c(
      "IPW, optimal weight", "AIPW, optimal weight",
      "IPW, zero weight", "AIPW, zero weight"
    )
  )

  report <- run_simulation(sim_obj, quiet = TRUE)

  # Structure
  expect_s4_class(report, "simulation_report_obj")

  tol <- 1e-4 # simulation uses only 3 trials, so fewer significant digits

  # Method descriptions
  expect_equal(
    slot(report, "method_description"),
    c(
      "IPW, optimal weight", "AIPW, optimal weight",
      "IPW, zero weight", "AIPW, zero weight"
    )
  )

  # Bias
  expect_equal(slot(report, "bias")[1], 0.71128272, tolerance = tol)
  expect_equal(slot(report, "bias")[2], 0.17113206, tolerance = tol)
  expect_equal(slot(report, "bias")[3], 0.74339702, tolerance = tol)
  expect_equal(slot(report, "bias")[4], 0.14498550, tolerance = tol)

  # Variance
  expect_equal(slot(report, "variance")[1], 1.28839749, tolerance = tol)
  expect_equal(slot(report, "variance")[2], 0.09574202, tolerance = tol)
  expect_equal(slot(report, "variance")[3], 1.34009734, tolerance = tol)
  expect_equal(slot(report, "variance")[4], 0.12189665, tolerance = tol)

  # MSE
  expect_equal(slot(report, "mse")[1], 1.79432060, tolerance = tol)
  expect_equal(slot(report, "mse")[2], 0.12502820, tolerance = tol)
  expect_equal(slot(report, "mse")[3], 1.89273648, tolerance = tol)
  expect_equal(slot(report, "mse")[4], 0.14291745, tolerance = tol)

  # Coverage, Type I error, Power
  expect_equal(slot(report, "coverage"), c(2 / 3, 1, 2 / 3, 1), tolerance = tol)
  expect_equal(slot(report, "type_I_error"), c(1 / 3, 0, 1 / 3, 0), tolerance = tol)
  expect_equal(slot(report, "power"), c(1, 1, 2 / 3, 1), tolerance = tol)
})

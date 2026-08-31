# Regression tests for the OLE Simulation Workflow vignette.
# Reproduces the exact simulation pipeline from set.seed(2023) and verifies
# that the simulation_report_obj slot values remain unchanged.
#
# Reference: vignettes/OLE_simulation_workflow.Rmd

# ---------- helper: generate vignette OLE simulation data -------------------
generate_OLE_sim_data <- function() {
  set.seed(2023)

  data_matrix_list <- list()
  ntrial <- 3
  true_effect_long <- 0

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
    model_form_x_t3 <- setNames(c(5.0, 1.9, 1.4, -1.3, -0.4, -0.15), varnames)
    model_form_x_t4 <- setNames(c(1.2, 1.0, 2.0, -0.5, -0.4, -0.10), varnames)

    outcome_model_specs <- list(
      list(
        effect = 0, model_form_x = model_form_x_t1,
        noise_mean = 0, noise_sd = 4
      ),
      list(
        effect = 0, model_form_x = model_form_x_t2,
        noise_mean = 0, noise_sd = 4
      ),
      list(
        effect = 0, model_form_x = model_form_x_t3,
        noise_mean = 0, noise_sd = 4
      ),
      list(
        effect = true_effect_long, model_form_x = model_form_x_t4,
        noise_mean = 0, noise_sd = 4
      )
    )

    data_matrix_list[[trial_iter]] <- simulate_trial(
      X_int, X_ext,
      num_treated = 150, OLE_flag = TRUE,
      T_cross = 2, outcome_model_specs
    )
  }

  data_matrix_list
}

# =============================================================================
# OLE simulation with DID methods (IPW, AIPW, OR) + bootstrap
# =============================================================================
test_that("OLE simulation report matches vignette", {
  sim_data <- generate_OLE_sim_data()

  model_form_mu <- c(
    "y1 ~ x1 + x2 + x3 + x4 + x5",
    "y2 ~ x1 + x2 + x3 + x4 + x5",
    "y3 ~ x1 + x2 + x3 + x4 + x5",
    "y4 ~ x1 + x2 + x3 + x4 + x5"
  )

  method_obj_list <- list(
    did_ec_ipw(
      ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
      trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
      bootstrap = 50
    ),
    did_ec_aipw(
      ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
      trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
      outcome_formula = model_form_mu,
      bootstrap = 50
    ),
    did_ec_or(
      outcome_formula_ext = model_form_mu,
      outcome_formula_rct_ctrl = model_form_mu,
      outcome_formula_rct_trt = model_form_mu,
      bootstrap = 50
    )
  )

  sim_obj <- setup_simulation_OLE(
    data_matrix_list      = sim_data,
    trial_status_col_name = "S",
    treatment_col_name    = "A",
    outcome_col_name      = c("y1", "y2", "y3", "y4"),
    covariates_col_name   = c("x1", "x2", "x3", "x4", "x5"),
    method_obj_list       = method_obj_list,
    true_effect           = 0,
    T_cross               = 2,
    alpha                 = 0.05,
    method_description    = c("IPW, DID", "AIPW, DID", "OR, DID")
  )

  report <- run_simulation(sim_obj, quiet = TRUE)

  # Structure
  expect_s4_class(report, "simulation_report_obj")

  tol <- 1e-4

  # Method descriptions
  expect_equal(
    slot(report, "method_description"),
    c("IPW, DID", "AIPW, DID", "OR, DID")
  )

  # Bias
  expect_equal(slot(report, "bias")[1], 0.76080101, tolerance = tol)
  expect_equal(slot(report, "bias")[2], 1.61689948, tolerance = tol)
  expect_equal(slot(report, "bias")[3], 0.78887430, tolerance = tol)

  # Variance
  expect_equal(slot(report, "variance")[1], 2.82083713, tolerance = tol)
  expect_equal(slot(report, "variance")[2], 2.89178746, tolerance = tol)
  expect_equal(slot(report, "variance")[3], 0.58242440, tolerance = tol)

  # MSE
  expect_equal(slot(report, "mse")[1], 3.39965530, tolerance = tol)
  expect_equal(slot(report, "mse")[2], 5.50615140, tolerance = tol)
  expect_equal(slot(report, "mse")[3], 1.20474706, tolerance = tol)

  # Coverage and Type I error
  expect_equal(slot(report, "coverage"), c(1, 1, 1))
  expect_equal(slot(report, "type_I_error"), c(0, 0, 0))

  # Power (OLE simulation has no alt hypothesis, so power slot is empty)
  expect_length(slot(report, "power"), 0)
})

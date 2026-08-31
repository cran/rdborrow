## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE, include=FALSE-----------------------
library(rdborrow)

## -----------------------------------------------------------------------------
# Initialize an empty data list
set.seed(2023)

data_matrix_list_null <- list()
data_matrix_list_alt <- list()
# a small ntrial keeps this vignette fast to build; use hundreds to
# thousands of trials for stable operating characteristic estimates
ntrial <- 20

# Specify the significance level alpha
alpha <- 0.05

# Specify the true effect size at the end of the study
true_effect <- 0
alt_effect <- 2.0 # tune this

# Specify column names
covariates_col_name <- c("x1", "x2", "x3", "x4", "x5")
outcome_col_name <- c("y1", "y2")
treatment_col_name <- "A"
trial_status_col_name <- "S"

## -----------------------------------------------------------------------------
# Sequentially adding in datasets
for (trial_iter in 1:ntrial) {
  # simulate 300 sample
  normal <- copula::normalCopula(param = c(0.8), dim = 4, dispstr = "ar1")

  # ========== generate internal covariates =============
  X_int <- simulate_X_copula(
    n = 200,
    p = 4,
    cp = normal, # copula
    margins = c("binom", "binom", "binom", "exp"), # specify marginal distributions
    paramMargins = list(
      list(size = 1, prob = 0.7), # specify parameters for marginals
      list(size = 1, prob = 0.9),
      list(size = 1, prob = 0.3),
      list(rate = 1 / 10)
    )
  )

  X_int$x4 <- round(X_int$x4) + 1
  X_int$x5 <- 30 + 10 * X_int$x1 + (7) * X_int$x2 + (-6) * X_int$x3 + (-0.5) * X_int$x4 + rnorm(200, mean = 0, sd = 10)

  varnames <- c("1", paste0("x", 1:5))

  # ============ generate external covariates ==============
  X_ext <- simulate_X_copula(
    n = 100,
    p = 4,
    cp = normal, # copula
    margins = c("binom", "binom", "binom", "exp"), # specify marginal distributions
    paramMargins = list(
      list(size = 1, prob = 0.7), # specify parameters for marginals
      list(size = 1, prob = 0.9),
      list(size = 1, prob = 0.3),
      list(rate = 1 / 10)
    )
  )

  X_ext$x4 <- round(X_ext$x4) + 1
  X_ext$x5 <- 50 + 10 * X_ext$x1 + (2) * X_ext$x2 + (-1) * X_ext$x3 + (-0.3) * X_ext$x4 + rnorm(100, mean = 0, sd = 10)

  varnames <- c("1", paste0("x", 1:5))

  # ============ Specify outcome models ==============
  model_form_x_t1 <- setNames(c(10.0, 0.05, -1.5, -1.0, -0.2, -0.1), varnames) # 1.5*A, sigma = 4.0
  model_form_x_t2 <- setNames(c(6.0, 0.5, -0.5, -1.0, -0.3, -0.06), varnames) # 1.8*A, sigma = 4.0
  # model_form_x_t3 = setNames(c(5.0, 1.9, 1.4, -1.3, -0.4, -0.15), varnames) # 1.6*A, sigma = 4.0
  # model_form_x_t4 = setNames(c(1.2, 1.0, 2.0, -0.5, -0.4, -0.10), varnames) # 2.5*A, sigma = 5.0

  outcome_model_specs <- list(
    list(
      effect = 0, model_form_x = model_form_x_t1, # from data: true_effect = 1.5
      noise_mean = 0, noise_sd = 4
    ), # model form for the first time point, given by model_form1
    list(
      effect = true_effect, model_form_x = model_form_x_t2, # from data: true_effect = 1.8
      noise_mean = 0, noise_sd = 4
    ) # model form for the second time point, given by model_form2
  )

  # =========== generate trial data ============
  Data <- simulate_trial(X_int,
    X_ext,
    num_treated = 100,
    OLE_flag = FALSE,
    T_cross = 2,
    outcome_model_specs
  )

  data_matrix_list_null[[trial_iter]] <- Data
}


head(data_matrix_list_null[[1]])

## -----------------------------------------------------------------------------
# Sequentially adding in datasets
for (trial_iter in 1:ntrial) {
  # simulate 300 sample
  normal <- copula::normalCopula(param = c(0.8), dim = 4, dispstr = "ar1")

  # ========== generate internal covariates =============
  X_int <- simulate_X_copula(
    n = 200,
    p = 4,
    cp = normal, # copula
    margins = c("binom", "binom", "binom", "exp"), # specify marginal distributions
    paramMargins = list(
      list(size = 1, prob = 0.7), # specify parameters for marginals
      list(size = 1, prob = 0.9),
      list(size = 1, prob = 0.3),
      list(rate = 1 / 10)
    )
  )

  X_int$x4 <- round(X_int$x4) + 1
  X_int$x5 <- 30 + 10 * X_int$x1 + (7) * X_int$x2 + (-6) * X_int$x3 + (-0.5) * X_int$x4 + rnorm(200, mean = 0, sd = 10)

  varnames <- c("1", paste0("x", 1:5))

  # ============ generate external covariates ==============
  X_ext <- simulate_X_copula(
    n = 100,
    p = 4,
    cp = normal, # copula
    margins = c("binom", "binom", "binom", "exp"), # specify marginal distributions
    paramMargins = list(
      list(size = 1, prob = 0.7), # specify parameters for marginals
      list(size = 1, prob = 0.9),
      list(size = 1, prob = 0.3),
      list(rate = 1 / 10)
    )
  )

  X_ext$x4 <- round(X_ext$x4) + 1
  X_ext$x5 <- 50 + 10 * X_ext$x1 + (2) * X_ext$x2 + (-1) * X_ext$x3 + (-0.3) * X_ext$x4 + rnorm(100, mean = 0, sd = 10)

  varnames <- c("1", paste0("x", 1:5))

  # ============ Specify outcome models ==============
  model_form_x_t1 <- setNames(c(10.0, 0.05, -1.5, -1.0, -0.2, -0.1), varnames) # 1.5*A, sigma = 4.0
  model_form_x_t2 <- setNames(c(6.0, 0.5, -0.5, -1.0, -0.3, -0.06), varnames) # 1.8*A, sigma = 4.0
  # model_form_x_t3 = setNames(c(5.0, 1.9, 1.4, -1.3, -0.4, -0.15), varnames) # 1.6*A, sigma = 4.0
  # model_form_x_t4 = setNames(c(1.2, 1.0, 2.0, -0.5, -0.4, -0.10), varnames) # 2.5*A, sigma = 5.0

  outcome_model_specs <- list(
    list(
      effect = 0, model_form_x = model_form_x_t1, # from data: true_effect = 1.5
      noise_mean = 0, noise_sd = 4
    ), # model form for the first time point, given by model_form1
    list(
      effect = alt_effect, model_form_x = model_form_x_t2, # from data: true_effect = 1.8
      noise_mean = 0, noise_sd = 4
    ) # model form for the second time point, given by model_form2
  )

  # =========== generate trial data ============
  Data <- simulate_trial(X_int,
    X_ext,
    num_treated = 100,
    OLE_flag = FALSE,
    T_cross = 2,
    outcome_model_specs
  )

  data_matrix_list_alt[[trial_iter]] <- Data
}

head(data_matrix_list_alt[[1]])

## -----------------------------------------------------------------------------
method_IPW_optimal_weight <- ec_ipw(
  ps_formula = "S ~ x1 + x2 + x3 + x4 + x5"
)

method_AIPW_optimal_weight <- ec_aipw(
  ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
  outcome_formula = c(
    "y1 ~ x1 + x2 + x3 + x4 + x5",
    "y2 ~ x1 + x2 + x3 + x4 + x5"
  )
)

method_IPW_zero_weight <- ec_ipw(
  ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
  weight = 0
)

method_AIPW_zero_weight <- ec_aipw(
  ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
  outcome_formula = c(
    "y1 ~ x1 + x2 + x3 + x4 + x5",
    "y2 ~ x1 + x2 + x3 + x4 + x5"
  ),
  weight = 0
)

method_obj_list <- list(
  method_IPW_optimal_weight,
  method_AIPW_optimal_weight,
  method_IPW_zero_weight,
  method_AIPW_zero_weight
)

## -----------------------------------------------------------------------------
# create a simulation object for primary analysis
simulation_primary_obj <- setup_simulation_primary(
  data_matrix_list_null = data_matrix_list_null, # two scenarios
  data_matrix_list_alt = data_matrix_list_alt,
  trial_status_col_name = trial_status_col_name,
  treatment_col_name = treatment_col_name,
  outcome_col_name = outcome_col_name,
  covariates_col_name = covariates_col_name,
  method_obj_list = method_obj_list,
  true_effect = true_effect,
  alt_effect = alt_effect,
  alpha = alpha,
  method_description = c(
    "IPW, optimal weight",
    "AIPW, optimal weight",
    "IPW, zero weight",
    "AIPW, zero weight"
  )
)

## -----------------------------------------------------------------------------
simulation_report <- run_simulation(simulation_primary_obj, quiet = TRUE)

## -----------------------------------------------------------------------------
simulation_report # Type I error and Power

## -----------------------------------------------------------------------------
method_IPW_optimal_weight <- ec_ipw(
  ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
  bootstrap = 50,
  bootstrap_ci_type = "perc"
)

method_AIPW_optimal_weight <- ec_aipw(
  ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
  outcome_formula = c(
    "y1 ~ x1 + x2 + x3 + x4 + x5",
    "y2 ~ x1 + x2 + x3 + x4 + x5 + y1"
  ),
  bootstrap = 50,
  bootstrap_ci_type = "perc"
)

method_IPW_zero_weight <- ec_ipw(
  ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
  weight = 0,
  bootstrap = 50,
  bootstrap_ci_type = "perc"
)

method_AIPW_zero_weight <- ec_aipw(
  ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
  outcome_formula = c(
    "y1 ~ x1 + x2 + x3 + x4 + x5",
    "y2 ~ x1 + x2 + x3 + x4 + x5 + y1"
  ),
  weight = 0,
  bootstrap = 50,
  bootstrap_ci_type = "perc"
)

method_obj_list <- list(
  method_IPW_optimal_weight,
  method_AIPW_optimal_weight,
  method_IPW_zero_weight,
  method_AIPW_zero_weight
)

## -----------------------------------------------------------------------------
# create a simulation object for primary analysis
simulation_primary_obj <- setup_simulation_primary(
  data_matrix_list_null = data_matrix_list_null,
  data_matrix_list_alt = data_matrix_list_alt,
  trial_status_col_name = trial_status_col_name,
  treatment_col_name = treatment_col_name,
  outcome_col_name = outcome_col_name,
  covariates_col_name = covariates_col_name,
  method_obj_list = method_obj_list,
  true_effect = true_effect,
  alt_effect = alt_effect,
  alpha = alpha,
  method_description = c(
    "IPW, optimal weight, bootstrap",
    "AIPW, optimal weight, bootstrap",
    "IPW, zero weight, bootstrap",
    "AIPW, zero weight, bootstrap"
  )
)

## -----------------------------------------------------------------------------
simulation_report_bs <- run_simulation(simulation_primary_obj, quiet = TRUE)

## -----------------------------------------------------------------------------
simulation_report_bs


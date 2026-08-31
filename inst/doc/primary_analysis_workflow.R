## ----setup, message=FALSE, warning=FALSE, include=FALSE-----------------------
library(rdborrow)

## -----------------------------------------------------------------------------
head(SyntheticData)

## -----------------------------------------------------------------------------
method <- ec_ipw(
  ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
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

run_analysis(analysis)

## -----------------------------------------------------------------------------
method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")

analysis <- setup_analysis_primary(
  data = SyntheticData,
  trial_status_col_name = "S",
  treatment_col_name = "A",
  outcome_col_name = c("y1", "y2"),
  covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
  method_weighting_obj = method
)

run_analysis(analysis)

## -----------------------------------------------------------------------------
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

run_analysis(analysis)

## -----------------------------------------------------------------------------
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

run_analysis(analysis)

## -----------------------------------------------------------------------------
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

run_analysis(analysis)

## -----------------------------------------------------------------------------
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

run_analysis(analysis)


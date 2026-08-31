## ----setup, message=FALSE, warning=FALSE, include=FALSE-----------------------
library(rdborrow)

## -----------------------------------------------------------------------------
head(SyntheticData[, c("A", "S", "y1", "y2", "y3", "y4")])

## ----message=FALSE, warning=FALSE---------------------------------------------
method <- did_ec_ipw(
  ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
  trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
  bootstrap = 50
)

analysis <- setup_analysis_OLE(
  data = SyntheticData,
  trial_status_col_name = "S",
  treatment_col_name = "A",
  outcome_col_name = c("y1", "y2", "y3", "y4"),
  covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
  T_cross = 2,
  method_OLE_obj = method
)

run_analysis(analysis)

## ----message=FALSE, warning=FALSE---------------------------------------------
model_forms <- c(
  "y1 ~ x1 + x2 + x3 + x4 + x5",
  "y2 ~ x1 + x2 + x3 + x4 + x5",
  "y3 ~ x1 + x2 + x3 + x4 + x5",
  "y4 ~ x1 + x2 + x3 + x4 + x5"
)

method <- did_ec_aipw(
  ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
  trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
  outcome_formula = model_forms,
  bootstrap = 50
)

analysis <- setup_analysis_OLE(
  data = SyntheticData,
  trial_status_col_name = "S",
  treatment_col_name = "A",
  outcome_col_name = c("y1", "y2", "y3", "y4"),
  covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
  T_cross = 2,
  method_OLE_obj = method
)

run_analysis(analysis)

## ----message=FALSE, warning=FALSE---------------------------------------------
model_forms <- c(
  "y1 ~ x1 + x2 + x3 + x4 + x5",
  "y2 ~ x1 + x2 + x3 + x4 + x5",
  "y3 ~ x1 + x2 + x3 + x4 + x5",
  "y4 ~ x1 + x2 + x3 + x4 + x5"
)

method <- did_ec_or(
  outcome_formula_ext = model_forms,
  outcome_formula_rct_ctrl = model_forms,
  outcome_formula_rct_trt = model_forms,
  bootstrap = 50
)

analysis <- setup_analysis_OLE(
  data = SyntheticData,
  trial_status_col_name = "S",
  treatment_col_name = "A",
  outcome_col_name = c("y1", "y2", "y3", "y4"),
  covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
  T_cross = 2,
  method_OLE_obj = method
)

run_analysis(analysis)

## ----message=FALSE, warning=FALSE---------------------------------------------
method <- scm(
  lambda_min = 0,
  lambda_max = 1e-3,
  nlambda = 2,
  bootstrap = 3,
  bootstrap_ci_type = "perc"
)

analysis <- setup_analysis_OLE(
  data = SyntheticData,
  trial_status_col_name = "S",
  treatment_col_name = "A",
  outcome_col_name = c("y1", "y2", "y3", "y4"),
  covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
  T_cross = 2,
  method_OLE_obj = method
)

run_analysis(analysis)


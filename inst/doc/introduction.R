## ----setup, include=FALSE-----------------------------------------------------
library(rdborrow)

## -----------------------------------------------------------------------------
# create an EC-IPW method with optimal borrowing weight
method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")

# set up the analysis
analysis <- setup_analysis_primary(
  data = SyntheticData,
  trial_status_col_name = "S",
  treatment_col_name = "A",
  outcome_col_name = c("y1", "y2"),
  covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
  method_weighting_obj = method
)

# run
results <- run_analysis(analysis)
results


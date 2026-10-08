# run a primary analysis on the SyntheticData layout----
run_primary <- function(d, method, alpha = 0.05, outcomes = c("y1", "y2"),
                        covariates = c("x1", "x2", "x3", "x4", "x5")) {
  run_analysis(setup_analysis_primary(
    data = d,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = outcomes,
    covariates_col_name = covariates,
    method_weighting_obj = method,
    alpha = alpha
  ))
}

# density-ratio weights for external controls----
#' Fit the trial-participation propensity model and form the density-ratio
#' weights used by every external-control weighting estimator.
#' shared by ec_ipw, ec_aipw, did_ec_ipw and did_ec_aipw.
#' @param df internal data frame.
#' @param ps_formula propensity score formula string.
#' @param S trial participation vector.
#' @return list with ps_model, pi_SX, pi_S and w00.
#' @noRd
.ec_weights <- function(df, ps_formula, S) {
  pi_S <- sum(S) / length(S)
  ps_model <- glm(as.formula(ps_formula), data = df, family = "binomial")
  pi_SX <- predict(ps_model, newdata = df, type = "response")

  list(
    ps_model = ps_model,
    pi_SX = pi_SX,
    pi_S = pi_S,
    w00 = (pi_SX / (1 - pi_SX)) * ((1 - pi_S) / pi_S)
  )
}

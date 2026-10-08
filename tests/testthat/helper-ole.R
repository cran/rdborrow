# unbalanced OLE data with n_time visits, a covariate shift between trial and
# external controls, and a constant study effect----
make_ole_data <- function(n_trt = 60, n_ctrl = 30, n_ext = 45, n_time = 5,
                          seed = 1) {
  set.seed(seed)
  n <- n_trt + n_ctrl + n_ext
  S <- rep(c(1, 1, 0), c(n_trt, n_ctrl, n_ext))
  A <- rep(c(1, 0, 0), c(n_trt, n_ctrl, n_ext))
  df <- data.frame(
    x1 = rnorm(n, mean = 0.5 * (S == 0)),
    x2 = rbinom(n, 1, 0.5),
    S = S,
    A = A
  )
  for (t in seq_len(n_time)) {
    df[[paste0("y", t)]] <- 0.5 * t + df$x1 + 0.5 * df$x2 + 1.5 * S +
      (1 + 0.5 * df$x1) * A * (t > 2) + rnorm(n)
  }
  df
}

ole_outcomes <- function(df) grep("^y[0-9]+$", names(df), value = TRUE)

# plain difference-in-differences of group means (no covariates)----
unadjusted_did <- function(df, T_cross) {
  Y <- as.matrix(df[, ole_outcomes(df)])
  ole <- (T_cross + 1):ncol(Y)
  trt <- df$S == 1 & df$A == 1
  ctrl <- df$S == 1 & df$A == 0
  ext <- df$S == 0
  pre <- function(g) mean(rowMeans(Y[g, seq_len(T_cross), drop = FALSE]))
  unname(colMeans(Y[trt, ole, drop = FALSE]) - colMeans(Y[ext, ole, drop = FALSE]) -
    (pre(ctrl) - pre(ext)))
}

# point estimates of the three DID estimators from their core functions----
did_tau <- function(estimator, df, T_cross, covs = "x1 + x2") {
  outcomes <- ole_outcomes(df)
  d <- .build_analysis_df(df, outcomes, "A", "S", c("x1", "x2"))
  Y <- as.matrix(d[, outcomes, drop = FALSE])
  ps <- paste("S ~", covs)
  of <- paste(outcomes, "~", covs)
  unname(switch(estimator,
    ipw = .did_ec_ipw_core(d, Y, d$S, d$A, T_cross, ps, NULL)$tau,
    aipw = .did_ec_aipw_core(d, Y, d$S, d$A, T_cross, ps, NULL, of)$tau,
    or = .did_ec_or_core(d, d$S, d$A, T_cross, of, of, of)$tau
  ))
}

ole_analysis <- function(df, method, T_cross, alpha = 0.05) {
  setup_analysis_OLE(
    data = df,
    trial_status_col_name = "S",
    treatment_col_name = "A",
    outcome_col_name = ole_outcomes(df),
    covariates_col_name = c("x1", "x2"),
    method_OLE_obj = method,
    T_cross = T_cross,
    alpha = alpha
  )
}

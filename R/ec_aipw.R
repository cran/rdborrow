#' @include ec_ipw.R
NULL

# S4 class definition
.ec_aipw_method <- setClass(
  "ec_aipw_method",
  contains = "method_weighting_obj",
  slots = c(
    ps_formula = "character",
    outcome_formula = "character",
    weight = "numericOrNULL"
  ),
  prototype = list(
    method_name = "EC-AIPW",
    ps_formula = "",
    outcome_formula = "",
    weight = NULL
  )
)

#' EC-AIPW method
#'
#' Creates a method object for augmented IPW estimation with external
#' control borrowing (Zhou et al., 2024). Augments the IPW estimator
#' with an outcome regression model for improved efficiency. Pass to
#' \code{\link{setup_analysis_primary}} and \code{\link{run_analysis}}.
#'
#' @param ps_formula Formula string for the propensity score model
#'   predicting trial participation.
#' @param outcome_formula Character vector of outcome model formulas,
#'   one per time point (e.g., \code{c("y1 ~ x1 + x2", "y2 ~ x1 + x2")}).
#' @param weight Borrowing weight. \code{NULL} (default) for data-adaptive
#'   optimal weight, \code{0} for RCT-only, or a value in (0, 1].
#' @param bootstrap Number of bootstrap replicates, or \code{NULL}
#'   (default) for sandwich variance with normal CIs.
#' @param bootstrap_ci_type Bootstrap CI type. Defaults to \code{"perc"}.
#'
#' @return An S4 object of class \code{ec_aipw_method}.
#'
#' @references
#' Zhou et al. (2024). Causal estimators for incorporating external
#' controls in randomized trials with longitudinal outcomes.
#' \emph{JRSS-A}. \doi{10.1093/jrsssa/qnae075}
#'
#' @export
#'
#' @examples
#' # optimal weight, sandwich SE
#' ec_aipw(
#'   ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
#'   outcome_formula = c(
#'     "y1 ~ x1 + x2 + x3 + x4 + x5",
#'     "y2 ~ x1 + x2 + x3 + x4 + x5"
#'   )
#' )
#'
#' # no borrowing
#' ec_aipw(
#'   ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
#'   outcome_formula = c(
#'     "y1 ~ x1 + x2 + x3 + x4 + x5",
#'     "y2 ~ x1 + x2 + x3 + x4 + x5"
#'   ),
#'   weight = 0
#' )
#'
#' # fixed weight with bootstrap
#' ec_aipw(
#'   ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
#'   outcome_formula = c(
#'     "y1 ~ x1 + x2 + x3 + x4 + x5",
#'     "y2 ~ x1 + x2 + x3 + x4 + x5"
#'   ),
#'   weight = 0.3,
#'   bootstrap = 500
#' )
ec_aipw <- function(ps_formula,
                    outcome_formula,
                    weight = NULL,
                    bootstrap = NULL,
                    bootstrap_ci_type = NULL) {
  checkmate::assert_string(ps_formula)
  checkmate::assert_character(outcome_formula, min.len = 1)
  checkmate::assert_number(weight, lower = 0, upper = 1, null.ok = TRUE)
  checkmate::assert_count(bootstrap, positive = TRUE, null.ok = TRUE)

  # bootstrap type
  if (!is.null(bootstrap) && is.null(bootstrap_ci_type)) {
    bootstrap_ci_type <- "perc"
  }
  if (!is.null(bootstrap_ci_type)) {
    checkmate::assert_choice(
      bootstrap_ci_type, c("perc", "bca", "norm", "basic", "stud")
    )
  }

  .ec_aipw_method(
    ps_formula = ps_formula,
    outcome_formula = outcome_formula,
    weight = weight,
    bootstrap = bootstrap,
    bootstrap_ci_type = bootstrap_ci_type,
    method_name = "EC-AIPW"
  )
}

#' @rdname estimate
setMethod("estimate", "ec_aipw_method", function(method, data, outcomes,
                                                 treatment, trial_status,
                                                 covariates, alpha = 0.05,
                                                 quiet = TRUE) {
  ps_formula <- sub("^[^~]*~", paste0(trial_status, " ~"), method@ps_formula)
  if (length(method@outcome_formula) != length(outcomes)) {
    stop(
      "outcome_formula must have one formula per outcome (got ",
      length(method@outcome_formula), " for ", length(outcomes), " outcomes).",
      call. = FALSE
    )
  }
  df <- .build_analysis_df(data, outcomes, treatment, trial_status, covariates)
  n_time <- length(outcomes)

  if (!quiet) cat("Running EC-AIPW estimator...\n")

  # point estimate + sandwich SE
  core <- .ec_aipw_core(
    df, outcomes, ps_formula,
    method@outcome_formula, method@weight
  )
  sd_tau <- .ec_aipw_se(df, core, n_time, method@outcome_formula)

  # format results
  tau <- core$tau
  borrow_weight <- core$borrow_weight
  cutoff <- qnorm(1 - alpha / 2)
  results <- data.frame(
    point_estimates = tau,
    standard_deviation = sd_tau,
    lower_CI_normal = tau - sd_tau * cutoff,
    upper_CI_normal = tau + sd_tau * cutoff,
    row.names = paste0("tau", seq_len(n_time))
  )

  # bootstrap (optional)
  if (!is.null(method@bootstrap)) {
    if (!quiet) cat("Running bootstrap inference...\n")
    boot_res <- .run_bootstrap(
      df = df, statistic = .ec_aipw_boot_statistic,
      n_estimates = n_time, bootstrap = method@bootstrap,
      bootstrap_ci_type = method@bootstrap_ci_type, alpha = alpha,
      borrow_wt = borrow_weight, outcomes = outcomes,
      covariates = covariates, ps_formula = ps_formula,
      outcome_formula = method@outcome_formula
    )
    results <- data.frame(
      point_estimates = tau,
      standard_deviation = boot_res$sd_boot,
      lower_CI_boot = boot_res$lower_ci,
      upper_CI_boot = boot_res$upper_ci,
      row.names = paste0("tau", seq_len(n_time))
    )
  }

  list(results = results, borrow_weight = borrow_weight)
})

#' EC-AIPW point estimate (Def 2, Eq 7).
#' fits PS + outcome models, uses residuals Y-mu(X) in place of Y,
#' computes mu11/mu10/mu00 and optimal weight.
#' shared by estimate() and bootstrap statistic.
#' @param df internal data frame.
#' @param outcomes outcome column names.
#' @param ps_formula propensity score formula string.
#' @param outcome_formula character vector of outcome model formulas.
#' @param weight fixed weight or NULL for optimal.
#' @return list with tau, borrow_weight, and model intermediates.
#' @noRd
.ec_aipw_core <- function(df, outcomes, ps_formula, outcome_formula, weight) {
  # see Zhou 2024a: Def 2 (Eq 7) for point estimate, Eq 11 for optimal weight

  Y <- as.matrix(df[, outcomes, drop = FALSE])
  S <- df$S
  A <- df$A
  n <- sum(S)
  N <- length(S)
  pi_S <- n / N
  n_time <- ncol(Y)

  # propensity score model
  ps_model <- glm(as.formula(ps_formula), data = df, family = "binomial")
  pi_SX <- predict(ps_model, newdata = df, type = "response")
  pi_A <- sum(A[S == 1]) / n

  # weights
  w11 <- 1 / pi_A
  w10 <- 1 / (1 - pi_A)
  w00 <- (pi_SX / (1 - pi_SX)) * ((1 - pi_S) / pi_S) # density ratio

  # outcome regression on controls, predict for all subjects
  Y0_models <- lapply(outcome_formula, \(f) {
    lm(as.formula(f), data = df[A == 0, , drop = FALSE])
  })
  Y0 <- vapply(seq_len(n_time), \(t) {
    predict(Y0_models[[t]], newdata = df)
  }, numeric(N))
  Yr <- Y - Y0

  # mu-hat components using residuals (Def 2, Eq 7)
  Yr_trt <- Yr[S == 1 & A == 1, , drop = FALSE]
  Yr_ctrl <- Yr[S == 1 & A == 0, , drop = FALSE]
  Yr_ext <- Yr[S == 0, , drop = FALSE]
  w00_ext <- w00[S == 0]

  mu1 <- colMeans(Yr_trt)
  mu10 <- colMeans(Yr_ctrl)
  mu00 <- colSums(w00_ext * Yr_ext) / sum(w00_ext)

  # optimal weight
  if (is.null(weight)) {
    num <- sum(rep(w10^2, nrow(Yr_ctrl))) / sum(rep(w10, nrow(Yr_ctrl)))^2
    denom <- sum(w00_ext^2) / sum(w00_ext)^2
    borrow_weight <- num / (num + denom)
  } else {
    borrow_weight <- weight
  }

  # combine trial and external controls
  mu0 <- (1 - borrow_weight) * mu10 + borrow_weight * mu00
  tau <- mu1 - mu0

  list(
    tau = tau, borrow_weight = borrow_weight,
    ps_model = ps_model, pi_SX = pi_SX, pi_A = pi_A,
    pi_S = pi_S, w00 = w00,
    Yr = Yr, mu1 = mu1, mu10 = mu10, mu00 = mu00
  )
}

#' sandwich variance for EC-AIPW.
#' extends the IPW sandwich with outcome model parameter blocks.
#' @param df internal data frame.
#' @param core output from .ec_aipw_core.
#' @param n_time number of time points.
#' @param outcome_formula character vector of outcome model formulas.
#' @return numeric vector of standard errors (length n_time).
#' @noRd
.ec_aipw_se <- function(df, core, n_time, outcome_formula) {
  # see Zhou 2024a: Theorem 4 (Eq 15 for A/B matrices, Eq 16 for variance)

  S <- df$S
  A <- df$A
  N <- nrow(df)

  X_ps <- model.matrix(core$ps_model)
  n_ps <- ncol(X_ps)

  # refit outcome models on full data (needed for sandwich, not for tau)
  Y0_models_full <- lapply(outcome_formula, \(f) {
    lm(as.formula(f), data = df)
  })

  # bread: ps block----
  A33 <- diag(
    rep(-mean((1 - S) * core$w00 / (1 - core$pi_S)), n_time),
    nrow = n_time
  )
  A34 <- t((1 - S) * core$pi_SX / (core$pi_S * (1 - core$pi_SX)) *
    sweep(core$Yr, 2, core$mu00)) %*% X_ps / N
  A44 <- t(X_ps) %*% diag(-core$pi_SX * (1 - core$pi_SX)) %*% X_ps / N

  A0 <- as.matrix(Matrix::bdiag(
    diag(-1, n_time), diag(-1, n_time), A33, A44
  ))
  A0[
    (2 * n_time + 1):(3 * n_time),
    (3 * n_time + 1):(3 * n_time + n_ps)
  ] <- A34

  # bread: outcome model blocks----
  Y0_model_mats <- lapply(Y0_models_full, model.matrix)
  n_outcome <- sum(vapply(Y0_model_mats, ncol, integer(1)))

  Phi1_gamma <- as.matrix(Matrix::bdiag(lapply(seq_len(n_time), \(t) {
    as.vector(-S * A / (core$pi_S * core$pi_A)) %*% Y0_model_mats[[t]] / N
  })))
  Phi2_gamma <- as.matrix(Matrix::bdiag(lapply(seq_len(n_time), \(t) {
    as.vector(-S * (1 - A) / (core$pi_S * (1 - core$pi_A))) %*%
      Y0_model_mats[[t]] / N
  })))
  Phi3_gamma <- as.matrix(Matrix::bdiag(lapply(seq_len(n_time), \(t) {
    as.vector(-(1 - S) * core$w00 / (1 - core$pi_S)) %*%
      Y0_model_mats[[t]] / N
  })))
  Y0_gamma <- as.matrix(Matrix::bdiag(lapply(seq_len(n_time), \(t) {
    -t(Y0_model_mats[[t]]) %*%
      diag((1 - A) / (1 - mean(A))) %*% Y0_model_mats[[t]] / N
  })))

  # assemble full bread matrix----
  A_left <- rbind(A0, matrix(0, nrow = n_outcome, ncol = 3 * n_time + n_ps))
  A_right <- rbind(
    Phi1_gamma, Phi2_gamma, Phi3_gamma,
    matrix(0, nrow = n_ps, ncol = n_outcome),
    Y0_gamma
  )
  A_mat <- cbind(A_left, A_right)

  # meat: influence functions----
  phi1 <- S * A * sweep(core$Yr, 2, core$mu1) / core$pi_A / core$pi_S
  phi2 <- S * (1 - A) * sweep(core$Yr, 2, core$mu10) /
    (1 - core$pi_A) / core$pi_S
  phi3 <- (1 - S) * sweep(core$Yr, 2, core$mu00) *
    core$w00 / (1 - core$pi_S)
  phi_ps <- (S - core$pi_SX) * X_ps
  phi_Y0 <- do.call(cbind, lapply(seq_len(n_time), \(t) {
    ((1 - A) / (1 - mean(A))) * (core$Yr[, t] * Y0_model_mats[[t]])
  }))

  B <- crossprod(cbind(phi1, phi2, phi3, phi_ps, phi_Y0)) / N

  # sandwich: A^{-1} B A^{-T}, then extract tau variance
  A_inv <- solve(A_mat)
  sigma <- A_inv %*% B %*% t(A_inv)

  coef_mat <- cbind(
    diag(n_time),
    -(1 - core$borrow_weight) * diag(n_time),
    -core$borrow_weight * diag(n_time),
    matrix(0, nrow = n_time, ncol = n_ps + n_outcome)
  )
  sqrt(diag(coef_mat %*% sigma %*% t(coef_mat) / N))
}

#' bootstrap statistic for EC-AIPW. called by boot::boot on each resample.
#' refits both PS and outcome models on the resampled data.
#' @param data internal data frame.
#' @param indices bootstrap sample indices.
#' @param outcomes outcome column names.
#' @param covariates covariate column names.
#' @param ps_formula propensity score formula.
#' @param outcome_formula outcome model formulas.
#' @param borrow_wt pre-computed borrowing weight.
#' @return numeric vector of tau estimates.
#' @noRd
.ec_aipw_boot_statistic <- function(data, indices, outcomes, covariates,
                                    ps_formula, outcome_formula, borrow_wt) {
  d <- data[indices, , drop = FALSE]
  core <- .ec_aipw_core(d, outcomes, ps_formula, outcome_formula, borrow_wt)
  core$tau
}

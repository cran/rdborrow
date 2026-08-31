#' @include did_ec_ipw.R
NULL

# S4 class definition----
.did_ec_aipw_method <- setClass(
  "did_ec_aipw_method",
  contains = "method_DID_obj",
  slots = c(
    ps_formula = "character",
    trt_formula = "characterOrNULL",
    outcome_formula = "character"
  ),
  prototype = list(
    method_name = "DID-EC-AIPW",
    ps_formula = "",
    trt_formula = NULL,
    outcome_formula = ""
  )
)

#' DID-EC-AIPW method
#'
#' Creates a method object for difference-in-differences augmented
#' inverse probability weighting (DID-EC-AIPW) estimation with
#' external control borrowing for the open-label extension phase
#' (Zhou et al., 2024, Eq. 5). Doubly robust: consistent if either
#' the propensity score model or the outcome model is correct.
#'
#' @param ps_formula Formula string for the propensity score model
#'   predicting trial participation.
#' @param trt_formula Formula string for the treatment assignment model,
#'   or \code{NULL} (default) for marginal probability.
#' @param outcome_formula Character vector of outcome model formulas,
#'   one per time point.
#' @param bootstrap Number of bootstrap replicates. Defaults to 500.
#' @param bootstrap_ci_type Bootstrap CI type. Defaults to \code{"perc"}.
#'
#' @return An S4 object of class \code{did_ec_aipw_method}.
#'
#' @references
#' Zhou et al. (2024). Estimating treatment effect in randomized trial
#' after control to treatment crossover using external controls.
#' \emph{Journal of Biopharmaceutical Statistics}.
#' \doi{10.1080/10543406.2024.2444222}
#'
#' @export
#'
#' @examples
#' did_ec_aipw(
#'   ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
#'   trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
#'   outcome_formula = c(
#'     "y1 ~ x1 + x2 + x3 + x4 + x5",
#'     "y2 ~ x1 + x2 + x3 + x4 + x5",
#'     "y3 ~ x1 + x2 + x3 + x4 + x5",
#'     "y4 ~ x1 + x2 + x3 + x4 + x5"
#'   ),
#'   bootstrap = 500
#' )
did_ec_aipw <- function(ps_formula,
                        trt_formula = NULL,
                        outcome_formula,
                        bootstrap = 500L,
                        bootstrap_ci_type = NULL) {
  checkmate::assert_string(ps_formula)
  checkmate::assert_string(trt_formula, null.ok = TRUE)
  checkmate::assert_character(outcome_formula, min.len = 1)
  checkmate::assert_count(bootstrap, positive = TRUE)

  if (is.null(bootstrap_ci_type)) {
    bootstrap_ci_type <- "perc"
  }
  checkmate::assert_choice(
    bootstrap_ci_type, c("perc", "bca", "norm", "basic", "stud")
  )

  .did_ec_aipw_method(
    ps_formula = ps_formula,
    trt_formula = trt_formula,
    outcome_formula = outcome_formula,
    bootstrap = bootstrap,
    bootstrap_ci_type = bootstrap_ci_type,
    method_name = "DID-EC-AIPW"
  )
}

#' @rdname estimate
setMethod("estimate", "did_ec_aipw_method", function(method, data, outcomes,
                                                     treatment, trial_status,
                                                     covariates, alpha = 0.05,
                                                     quiet = TRUE,
                                                     T_cross) {
  df <- .build_analysis_df(data, outcomes, treatment, trial_status, covariates)
  Y <- as.matrix(df[, outcomes, drop = FALSE])
  S <- df$S
  A <- df$A

  ps_formula <- sub("^[^~]*~", paste0(trial_status, " ~"), method@ps_formula)
  trt_formula <- method@trt_formula
  if (!is.null(trt_formula)) {
    trt_formula <- sub("^[^~]*~", paste0(treatment, " ~"), trt_formula)
  }

  if (!quiet) cat("Running DID-EC-AIPW estimator...\n")

  result <- .did_ec_aipw_core(
    df, Y, S, A, T_cross, ps_formula, trt_formula, method@outcome_formula
  )
  tau <- result$tau

  if (!quiet) cat("Running bootstrap inference...\n")

  n_ole <- ncol(Y) - T_cross
  boot_res <- .run_bootstrap(
    df = df, statistic = .did_ec_aipw_boot_statistic,
    n_estimates = n_ole, bootstrap = method@bootstrap,
    bootstrap_ci_type = method@bootstrap_ci_type, alpha = alpha,
    outcomes = outcomes, ps_formula = ps_formula,
    trt_formula = trt_formula, outcome_formula = method@outcome_formula,
    T_cross = T_cross
  )

  data.frame(
    point_estimates = tau,
    lower_CI_boot = boot_res$lower_ci,
    upper_CI_boot = boot_res$upper_ci,
    row.names = paste0("tau", (T_cross + 1):ncol(Y))
  )
})

# internal helpers----

#' DID-EC-AIPW point estimate (Zhou 2024b, Eq 5 / Appendix B).
#' @param df internal data frame.
#' @param Y outcome matrix (N x T).
#' @param S trial participation vector.
#' @param A treatment vector.
#' @param T_cross crossover time point.
#' @param ps_formula propensity score formula.
#' @param trt_formula treatment assignment formula (NULL for marginal).
#' @param outcome_formula character vector of outcome model formulas.
#' @return list with tau vector.
#' @noRd
.did_ec_aipw_core <- function(df, Y, S, A, T_cross, ps_formula,
                              trt_formula, outcome_formula) {
  # see Zhou 2024b: Eq 5 (identification), Appendix B (sample estimator)

  n <- sum(S)
  N <- length(S)
  pi_S <- n / N
  n_time <- ncol(Y)

  # propensity score model
  ps_model <- glm(as.formula(ps_formula), data = df, family = "binomial")
  pi_SX <- predict(ps_model, newdata = df, type = "response")

  # treatment assignment model
  if (is.null(trt_formula)) {
    pi_AX <- sum(A[S == 1]) / n
  } else {
    trt_model <- glm(as.formula(trt_formula),
      data = df[S == 1, , drop = FALSE], family = "binomial"
    )
    pi_AX <- predict(trt_model, newdata = df, type = "response")
  }

  # outcome models on external controls
  Y0_models <- lapply(outcome_formula, \(f) {
    lm(as.formula(f), data = df[S == 0, , drop = FALSE])
  })
  Y0 <- vapply(seq_len(n_time), \(t) {
    predict(Y0_models[[t]], newdata = df)
  }, numeric(N))
  Yr <- Y - Y0

  # weights
  w11 <- 1 / pi_AX
  w10 <- 1 / (1 - pi_AX)
  w00 <- (pi_SX / (1 - pi_SX)) * ((1 - pi_S) / pi_S)

  # normalized weighted residuals per group
  Yr_trt <- Yr[S == 1 & A == 1, , drop = FALSE]
  Yr_ctrl <- Yr[S == 1 & A == 0, , drop = FALSE]
  Yr_ext <- Yr[S == 0, , drop = FALSE]
  w11_trt <- w11[S == 1 & A == 1]
  w10_ctrl <- w10[S == 1 & A == 0]
  w00_ext <- w00[S == 0]

  # delta_trial: treated OLE residuals
  mu_trt_ole <- colSums(w11_trt * Yr_trt[, (T_cross + 1):n_time, drop = FALSE]) /
    sum(w11_trt)

  # delta_EC: external control OLE residuals
  mu_ext_ole <- colSums(w00_ext * Yr_ext[, (T_cross + 1):n_time, drop = FALSE]) /
    sum(w00_ext)

  # bias correction: pre-crossover difference in residuals
  bias_ctrl <- sum(w10_ctrl * rowMeans(Yr_ctrl[, 1:T_cross, drop = FALSE])) /
    sum(w10_ctrl)
  bias_ext <- sum(w00_ext * rowMeans(Yr_ext[, 1:T_cross, drop = FALSE])) /
    sum(w00_ext)
  bias <- bias_ctrl - bias_ext

  tau <- mu_trt_ole - mu_ext_ole - bias
  list(tau = tau)
}

#' bootstrap statistic for DID-EC-AIPW.
#' @param data internal data frame.
#' @param indices bootstrap sample indices.
#' @param outcomes outcome column names.
#' @param ps_formula propensity score formula.
#' @param trt_formula treatment assignment formula.
#' @param outcome_formula outcome model formulas.
#' @param T_cross crossover time point.
#' @return numeric vector of tau estimates.
#' @noRd
.did_ec_aipw_boot_statistic <- function(data, indices, outcomes, ps_formula,
                                        trt_formula, outcome_formula, T_cross) {
  d <- data[indices, , drop = FALSE]
  Y <- as.matrix(d[, outcomes, drop = FALSE])
  .did_ec_aipw_core(
    d, Y, d$S, d$A, T_cross, ps_formula, trt_formula,
    outcome_formula
  )$tau
}

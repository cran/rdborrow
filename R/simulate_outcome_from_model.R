#' Simulate outcomes from additive linear models
#'
#' Generates longitudinal outcomes from a structural causal model of
#' the form `Y_t = A * effect + X %*% coef + noise`. In the primary
#' phase (`t <= T_cross`), treatment assignment `A` is used directly.
#' In the OLE phase (`t > T_cross`), all RCT patients are assumed to
#' receive treatment (effect multiplied by 1 instead of `A`).
#'
#' Each element of `outcome_model_specs` is a list with:
#' \describe{
#'   \item{`effect`}{Numeric scalar. Treatment effect for this time
#'     point.}
#'   \item{`model_form_x`}{Named numeric vector of covariate
#'     coefficients. Names must include `"1"` (intercept) and match
#'     column names in `X`.}
#'   \item{`noise_mean`}{Numeric scalar. Mean of the normal noise.}
#'   \item{`noise_sd`}{Numeric scalar. SD of the normal noise.}
#' }
#'
#' @param X Data frame of covariates.
#' @param A Numeric vector of treatment indicators (same length as
#'   `nrow(X)`).
#' @param outcome_model_specs List of lists, one per time point. See
#'   Details above.
#' @param OLE_flag Logical. If `TRUE`, time points after `T_cross`
#'   use the OLE model (all patients treated).
#' @param T_cross Positive integer. The crossover time point
#'   separating the primary and OLE phases. Only used when
#'   `OLE_flag = TRUE`.
#'
#' @return A data frame with `n` rows and one column per time point
#'   (`y1`, `y2`, ...).
#' @export
#'
#' @examples
#' X <- data.frame(x1 = rnorm(20), x2 = rnorm(20))
#' A <- rbinom(20, 1, 0.5)
#' specs <- list(
#'   list(
#'     effect = 1.5,
#'     model_form_x = c("1" = 2.0, "x1" = 0.5, "x2" = -0.3),
#'     noise_mean = 0, noise_sd = 1
#'   ),
#'   list(
#'     effect = 0,
#'     model_form_x = c("1" = 1.0, "x1" = 0.2, "x2" = 0.1),
#'     noise_mean = 0, noise_sd = 1
#'   )
#' )
#' Y <- simulate_outcome_from_model(X, A, specs, OLE_flag = FALSE, T_cross = 2)
simulate_outcome_from_model <- function(X, A, outcome_model_specs, OLE_flag, T_cross) {
  # validate inputs----
  checkmate::assert_data_frame(X, min.rows = 1)
  checkmate::assert_numeric(A, len = nrow(X))
  checkmate::assert_list(outcome_model_specs, min.len = 1)
  checkmate::assert_flag(OLE_flag)
  checkmate::assert_count(T_cross, positive = TRUE)

  n <- nrow(X)
  T_follow <- length(outcome_model_specs)

  # build the design matrix with intercept----
  X_design <- as.matrix(cbind("1" = 1, X))

  # helper to compute outcome for one time point
  compute_outcome <- function(spec, trt_indicator) {
    coefs <- spec$model_form_x
    linear_pred <- trt_indicator * spec$effect + X_design[, names(coefs)] %*% coefs
    as.numeric(linear_pred) + stats::rnorm(n, mean = spec$noise_mean, sd = spec$noise_sd)
  }

  # simulate outcomes at each time point----
  Y_list <- vector("list", T_follow)

  for (t in seq_len(T_follow)) {
    # in the OLE phase, all RCT patients receive treatment
    trt <- if (OLE_flag && t > T_cross) rep(1, n) else A
    Y_list[[t]] <- compute_outcome(outcome_model_specs[[t]], trt)
  }

  Y <- data.frame(Y_list)
  colnames(Y) <- paste0("y", seq_len(T_follow))

  Y
}

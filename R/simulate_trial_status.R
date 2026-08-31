#' Simulate trial participation status
#'
#' Simulates a binary trial participation indicator using a logistic
#' model. The probability of participation is
#' `inv.logit(X_intercept %*% coef)`, where `X_intercept` is the
#' covariate matrix with an intercept column prepended.
#'
#' @param X Data frame of covariates. The number of columns must
#'   equal `length(model_specs$coef) - 1` (the intercept is added
#'   automatically).
#' @param model_specs List with:
#'   \describe{
#'     \item{`family`}{Character string. Currently only `"binomial"`
#'       is supported.}
#'     \item{`coef`}{Numeric vector of length `ncol(X) + 1`.
#'       Logistic regression coefficients (intercept first).}
#'   }
#'
#' @return A single-column data frame with column `S` (1 = RCT
#'   participant, 0 = external control).
#' @export
#'
#' @examples
#' X <- data.frame(x1 = rnorm(20), x2 = rnorm(20))
#' S <- simulate_trial_status(X, model_specs = list(
#'   family = "binomial",
#'   coef = c(0, 0.5, -0.5)
#' ))
simulate_trial_status <- function(X, model_specs) {
  # validate inputs----
  checkmate::assert_data_frame(X, min.rows = 1)
  checkmate::assert_list(model_specs)
  checkmate::assert_choice(model_specs$family, choices = "binomial")
  checkmate::assert_numeric(model_specs$coef, len = ncol(X) + 1)

  # compute participation probability and draw----
  n <- nrow(X)
  X_intercept <- cbind(intercept = 1, X)
  beta <- model_specs$coef
  piS <- inv.logit(as.matrix(X_intercept) %*% beta)

  S <- rbinom(n, size = 1, prob = piS)
  data.frame(S = S)
}

#' Simulate treatment assignment
#'
#' Randomly assigns treatment to RCT patients (`S = 1`) with a given
#' probability. External control patients (`S = 0`) always receive
#' control (`A = 0`).
#'
#' @param X Data frame of covariates. Must have the same number of
#'   rows as `S`.
#' @param S Data frame with a column `S` indicating trial status
#'   (1 = RCT, 0 = external control).
#' @param prob Numeric scalar between 0 and 1. Probability of
#'   treatment assignment for RCT patients.
#'
#' @return A single-column data frame with column `A` indicating
#'   treatment status (1 = treated, 0 = control).
#' @export
#'
#' @examples
#' X <- SyntheticData[c("x1", "x2")]
#' S <- SyntheticData["S"]
#' A <- simulate_trt_assign(X, S, prob = 1 / 2)
simulate_trt_assign <- function(X, S, prob) {
  # validate inputs----
  checkmate::assert_data_frame(X)
  checkmate::assert_data_frame(S)
  checkmate::assert_number(prob, lower = 0, upper = 1)
  if (nrow(X) != nrow(S)) {
    stop("`X` and `S` must have the same number of rows.")
  }

  # assign treatment----
  # external controls (S = 0) always get A = 0;
  # RCT patients (S = 1) are randomized with probability prob
  n <- nrow(X)
  A <- S$S * rbinom(n, size = 1, prob = prob)
  data.frame(A = A)
}

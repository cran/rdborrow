#' Simulate covariates by discretizing a multivariate normal
#'
#' Draws from a multivariate normal distribution and optionally
#' discretizes selected columns into categorical variables based on
#' specified probability cutpoints.
#'
#' @param n Positive integer. Number of units to simulate.
#' @param p Positive integer. Number of covariates.
#' @param mu Numeric vector of length `p`. Mean of the multivariate
#'   normal. Defaults to a zero vector.
#' @param sig Numeric matrix of dimension `p x p`. Covariance matrix.
#'   Defaults to the identity matrix.
#' @param cat_cols Integer vector. Column indices to discretize into
#'   categorical variables. Each index must be between 1 and `p`.
#' @param cat_prob List of numeric vectors, one per element of
#'   `cat_cols`. Each vector gives the category probabilities (must
#'   sum to 1). The number of categories equals the length of the
#'   vector, and the resulting values are 0-indexed (0, 1, ..., K-1).
#'
#' @return A data frame with `n` rows and `p` columns named `x1`,
#'   ..., `xp`.
#' @export
#'
#' @examples
#' simulate_X_dct_mvnorm(
#'   20, 3,
#'   mu = rep(0, 3), sig = diag(3),
#'   cat_cols = c(1),
#'   cat_prob = list(c(0.3, 0.7))
#' )
#' simulate_X_dct_mvnorm(
#'   20, 3,
#'   mu = rep(0, 3), sig = diag(3),
#'   cat_cols = c(1, 3),
#'   cat_prob = list(c(0.2, 0.6, 0.2), c(0.3, 0.7))
#' )
simulate_X_dct_mvnorm <- function(n, p, mu = rep(0, p), sig = diag(p),
                                  cat_cols = c(), cat_prob = list()) {
  # validate inputs----
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_count(p, positive = TRUE)
  checkmate::assert_numeric(mu, len = p)
  checkmate::assert_matrix(sig, nrows = p, ncols = p)
  checkmate::assert_integerish(cat_cols,
    lower = 1, upper = p, any.missing = FALSE,
    null.ok = TRUE
  )
  checkmate::assert_list(cat_prob, len = length(cat_cols))

  # draw from multivariate normal----
  p_cat <- length(cat_cols)
  covariate <- rmvnorm(n, mean = mu, sigma = sig)

  # discretize selected columns----
  # uses normal quantiles from cat_prob as cutpoints to bin into 0, 1, ..., K-1
  if (p_cat > 0) {
    covariate[, cat_cols] <- sapply(1:p_cat, function(k) {
      cut_val <- qnorm(
        cumsum(cat_prob[[k]]),
        mean = mu[cat_cols[k]],
        sd = sqrt(sig[cat_cols[k], cat_cols[k]])
      )
      sapply(1:n, function(i) {
        which(covariate[i, cat_cols[k]] < cut_val)[1] - 1
      })
    })
  }

  colnames(covariate) <- paste0("x", 1:p)
  data.frame(covariate)
}

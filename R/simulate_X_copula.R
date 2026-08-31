#' Simulate covariates using a copula
#'
#' Couples several marginal distributions using a copula to generate
#' correlated multivariate covariates.
#'
#' @param n Positive integer. Number of units to simulate.
#' @param p Positive integer. Number of covariates. Must equal the
#'   dimension of `cp`.
#' @param cp A copula object (from the `copula` package).
#' @param margins Character vector of length `p`. Names of marginal
#'   distributions (e.g. `"norm"`, `"binom"`).
#' @param paramMargins List of length `p`. Each element is a named list
#'   of parameters for the corresponding marginal distribution.
#'
#' @return A data frame with `n` rows and `p` columns named `x1`,
#'   ..., `xp`.
#' @export
#'
#' @examples
#' normal <- copula::normalCopula(param = c(0.8), dim = 4, dispstr = "ar1")
#' X <- simulate_X_copula(1000, 4, normal,
#'   margins = c("norm", "t", "norm", "binom"),
#'   paramMargins = list(
#'     list(mean = 2, sd = 3),
#'     list(df = 2),
#'     list(mean = 0, sd = 1),
#'     list(size = 10, prob = 0.5)
#'   )
#' )
#' cor(X, method = "spearman")
simulate_X_copula <- function(n, p, cp, margins, paramMargins) {
  # validate inputs----
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_count(p, positive = TRUE)
  checkmate::assert_class(cp, "copula")
  checkmate::assert_character(margins, len = p)
  checkmate::assert_list(paramMargins, len = p)
  if (cp@dimension != p) {
    stop(paste0(
      "`p` must equal the dimension of `cp` (", cp@dimension, "), not ", p, "."
    ))
  }

  # build multivariate distribution and sample----
  multivariate_dist <- mvdc(
    copula = cp,
    margins = margins,
    paramMargins = paramMargins
  )

  covariate <- rMvdc(n, multivariate_dist)
  colnames(covariate) <- paste0("x", 1:p)
  data.frame(covariate)
}

#' Simulate covariates from a Gaussian mixture model
#'
#' Generates covariates where categorical variables define mixture
#' components and continuous variables are drawn from
#' component-specific multivariate normal distributions. Each
#' combination of categorical levels has its own probability and its
#' own distribution for the continuous covariates.
#'
#' @param n Positive integer. Number of units to simulate.
#' @param p_cat Non-negative integer. Number of categorical
#'   covariates.
#' @param p_cont Non-negative integer. Number of continuous
#'   covariates. At least one of `p_cat` or `p_cont` must be
#'   positive.
#' @param cat_level_list List of length `p_cat`. Each element is a
#'   vector of possible levels for that categorical variable. The
#'   total number of combinations is
#'   `prod(lengths(cat_level_list))`.
#' @param cat_comb_prob Numeric vector of probabilities, one per
#'   combination of categorical levels (in the order produced by
#'   [expand.grid()]). Must sum to 1.
#' @param cont_para_list List of parameter lists for the continuous
#'   covariates. When `p_cat > 0`, must have one element per
#'   combination of categorical levels; each element is a list with
#'   `mean` (length `p_cont`) and `sigma` (`p_cont x p_cont`
#'   matrix). When `p_cat == 0`, a single-element list.
#'
#' @return A data frame with `n` rows and `p_cat + p_cont` columns
#'   named `x1`, ..., `xp`.
#' @export
#'
#' @examples
#' # Continuous only
#' X <- simulate_X_mixture(
#'   n = 100, p_cat = 0, p_cont = 2,
#'   cat_level_list = list(),
#'   cat_comb_prob = c(),
#'   cont_para_list = list(list(mean = c(0, 0), sigma = diag(2)))
#' )
#'
#' # Mixed categorical and continuous
#' X <- simulate_X_mixture(
#'   n = 100, p_cat = 1, p_cont = 2,
#'   cat_level_list = list(c(0, 1)),
#'   cat_comb_prob = c(0.4, 0.6),
#'   cont_para_list = list(
#'     list(mean = c(0, 0), sigma = diag(2)),
#'     list(mean = c(2, 2), sigma = diag(2))
#'   )
#' )
simulate_X_mixture <- function(n, p_cat, p_cont, cat_level_list,
                               cat_comb_prob, cont_para_list) {
  # validate inputs----
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_count(p_cat)
  checkmate::assert_count(p_cont)
  if (p_cat + p_cont == 0) {
    stop("At least one of `p_cat` or `p_cont` must be positive.")
  }
  checkmate::assert_list(cat_level_list, len = p_cat)
  num_comb <- if (p_cat > 0) prod(lengths(cat_level_list)) else 1L

  if (p_cat > 0) {
    checkmate::assert_numeric(cat_comb_prob, len = num_comb)
  }
  if (p_cont > 0) {
    checkmate::assert_list(cont_para_list, len = num_comb)
  }

  p <- p_cat + p_cont

  # continuous only, single component----
  if (p_cat == 0) {
    covariate <- data.frame(rmvnorm(
      n,
      mean = cont_para_list[[1]]$mean,
      sigma = cont_para_list[[1]]$sigma
    ))
  } else {
    # assign patients to categorical combinations----
    N <- c(rmultinom(1, n, cat_comb_prob))

    covariate_cat <- expand.grid(cat_level_list)
    covariate_cat <- covariate_cat |>
      mutate(count = N) |>
      uncount(count)

    # draw continuous covariates per component----
    if (p_cont == 0) {
      covariate <- covariate_cat
    } else {
      covariate_cont <- bind_rows(lapply(1:num_comb, function(k) {
        data.frame(rmvnorm(
          N[k],
          mean = cont_para_list[[k]]$mean,
          sigma = cont_para_list[[k]]$sigma
        ))
      }))
      covariate <- cbind(covariate_cat, covariate_cont)
    }
  }

  colnames(covariate) <- paste0("x", 1:p)
  covariate
}

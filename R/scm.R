#' @include did_ec_ipw.R
NULL

# s4 class definition----
.scm_method <- setClass(
  "scm_method",
  contains = "method_SCM_obj",
  slots = c(
    lambda_min = "numeric",
    lambda_max = "numeric",
    nlambda = "integer",
    parallel = "character",
    ncpus = "integer"
  ),
  prototype = list(
    method_name = "SCM",
    lambda_min = 0,
    lambda_max = 0.1,
    nlambda = 2L,
    parallel = "no",
    ncpus = 1L
  )
)

# constructor----

#' SCM method constructor
#'
#' Creates a method object for synthetic control estimation with
#' external control borrowing for the open-label extension phase
#' (Zhou et al., 2024). Constructs a weighted combination of external
#' controls matching each RCT control subject on covariates and
#' pre-crossover outcomes. The penalty is chosen by leave-one-out
#' cross-validation over the external controls, so the data needs at least 2
#' external controls. The covariates must be numeric or logical; code a
#' factor as 0/1 indicators first.
#'
#' @param lambda_min Minimum penalty parameter for LOOCV. A finite number of
#'   at least 0.
#' @param lambda_max Maximum penalty parameter for LOOCV. A finite number of
#'   at least \code{lambda_min}.
#' @param nlambda Number of lambda values to evaluate in LOOCV, evenly spaced
#'   from \code{lambda_min} to \code{lambda_max}. With \code{nlambda = 1},
#'   \code{lambda_max} must equal \code{lambda_min}.
#' @param parallel Parallelization type for bootstrap (\code{"no"},
#'   \code{"multicore"}, or \code{"snow"}).
#' @param ncpus Number of CPUs for parallel bootstrap.
#' @param bootstrap Number of bootstrap replicates (at least 2).
#'   Defaults to 200.
#'   Use about 1000 or more for reported intervals; small values are for
#'   quick checks only, and their interval can exclude the point estimate.
#' @param bootstrap_ci_type Bootstrap CI type: one of \code{"perc"}
#'   (default), \code{"bca"}, \code{"norm"}, or \code{"basic"}.
#'   \code{"bca"} is slow when \code{bootstrap} is smaller than the number
#'   of patients: \code{boot::boot.ci()} then refits the estimator once for
#'   each patient.
#'
#' @details The analysis result from \code{\link{run_analysis}()} has an
#'   attribute \code{"lambda"}, the penalty that cross-validation selected:
#'   \code{attr(result, "lambda")}.
#'
#' @return An S4 object of class \code{scm_method}.
#'
#' @references
#' Zhou et al. (2024). Estimating treatment effect in randomized trial
#' after control to treatment crossover using external controls.
#' \emph{Journal of Biopharmaceutical Statistics}.
#' \doi{10.1080/10543406.2024.2330209}
#'
#' @export
#'
#' @examples
#' scm(lambda_min = 0, lambda_max = 0.001, nlambda = 2, bootstrap = 50)
scm <- function(lambda_min = 0,
                lambda_max = 0.1,
                nlambda = 2L,
                parallel = "no",
                ncpus = 1L,
                bootstrap = 200L,
                bootstrap_ci_type = NULL) {
  checkmate::assert_number(lambda_min, lower = 0, finite = TRUE)
  checkmate::assert_number(lambda_max, lower = lambda_min, finite = TRUE)
  checkmate::assert_count(nlambda, positive = TRUE)
  if (nlambda == 1 && lambda_max != lambda_min) {
    stop("With nlambda = 1, scm() uses only lambda_min. Set lambda_max equal ",
      "to lambda_min, or set nlambda to 2 or more.",
      call. = FALSE
    )
  }
  checkmate::assert_choice(parallel, c("no", "multicore", "snow"))
  checkmate::assert_count(ncpus, positive = TRUE)
  checkmate::assert_int(bootstrap, lower = 2)

  if (is.null(bootstrap_ci_type)) {
    bootstrap_ci_type <- "perc"
  }
  checkmate::assert_choice(
    bootstrap_ci_type, c("perc", "bca", "norm", "basic")
  )

  .scm_method(
    lambda_min = lambda_min,
    lambda_max = lambda_max,
    nlambda = as.integer(nlambda),
    parallel = parallel,
    ncpus = as.integer(ncpus),
    bootstrap = as.integer(bootstrap),
    bootstrap_ci_type = bootstrap_ci_type,
    method_name = "SCM"
  )
}

# estimate() method----

#' @rdname estimate
setMethod("estimate", "scm_method", function(method, data, outcomes,
                                             treatment, trial_status,
                                             covariates, alpha = 0.05,
                                             quiet = TRUE,
                                             T_cross) {
  if (!requireNamespace("ECOSolveR", quietly = TRUE)) {
    stop(
      "The 'ECOSolveR' package is required to fit the synthetic control ",
      "method (scm()). Install it with install.packages('ECOSolveR').",
      call. = FALSE
    )
  }
  .check_alpha(alpha)
  T_cross <- .check_T_cross(T_cross, outcomes)
  df <- .build_analysis_df(data, outcomes, treatment, trial_status, covariates)
  if (sum(df$S == 0) < 2) {
    stop("scm() needs at least 2 external controls for its leave-one-out ",
      "cross-validation; the data has ", sum(df$S == 0), ".",
      call. = FALSE
    )
  }
  not_numeric <- covariates[
    !vapply(df[covariates], \(x) is.numeric(x) || is.logical(x), logical(1))
  ]
  if (length(not_numeric) > 0) {
    stop("scm() matches patients on numeric covariates only. Convert these ",
      "covariates to numeric (for example, 0/1 indicators for each level): ",
      paste(not_numeric, collapse = ", "), ".",
      call. = FALSE
    )
  }
  n_time <- length(outcomes)
  long_term_col_name <- outcomes[(T_cross + 1):n_time]

  if (!quiet) message("Running the synthetic control method...")

  # find optimal lambda via LOOCV
  if (!quiet) message("Performing cross validation for tuning parameter selection...")
  lambda <- .scm_lambdacv(
    ec = .scm_matrices(df, outcomes, covariates)$X00,
    long_term_col_name = long_term_col_name,
    lambda_min = method@lambda_min,
    lambda_max = method@lambda_max,
    nlambda = method@nlambda
  )

  if (!quiet) message("Constructing pseudo controls for internal data...")
  tau <- .scm_core(df, outcomes, covariates, T_cross, lambda)$tau

  # bootstrap inference
  if (!quiet) message("Performing bootstrap inference...")
  n_ole <- n_time - T_cross
  boot_res <- .run_bootstrap(
    df = df, statistic = .scm_boot_statistic,
    n_estimates = n_ole, bootstrap = method@bootstrap,
    bootstrap_ci_type = method@bootstrap_ci_type, alpha = alpha,
    parallel = method@parallel, ncpus = method@ncpus,
    outcomes = outcomes, covariates = covariates,
    T_cross = T_cross, lambda = lambda
  )

  result <- data.frame(
    point_estimates = tau,
    standard_deviation = boot_res$sd_boot,
    lower_CI_boot = boot_res$lower_ci,
    upper_CI_boot = boot_res$upper_ci,
    row.names = paste0("tau", (T_cross + 1):n_time)
  )
  attr(result, "lambda") <- lambda
  result
})

# internal helpers----

#' solve penalized SC optimization for one RCT control subject.
#' finds convex weights over EC subjects minimizing squared distance
#' in covariates + pre-crossover outcomes, with penalty for dissimilarity.
#' @param subject integer index of the target RCT control subject.
#' @param X10 attribute matrix for RCT controls (features x subjects).
#' @param X00 attribute matrix for external controls (features x subjects).
#' @param long_term_col_name names of OLE outcome rows.
#' @param lambda penalty parameter.
#' @return list with (1) weight vector and (2) predicted OLE outcomes.
#' @noRd
.scm_subject_sc <- function(subject, X10, X00, long_term_col_name, lambda) {
  x1 <- X10[-which(row.names(X10) %in% long_term_col_name), subject]
  X0 <- X00[-which(row.names(X10) %in% long_term_col_name), , drop = FALSE]

  w <- CVXR::Variable(dim(X00)[2])
  loss <- sum(((x1 - X0 %*% w))^2)
  penal <- lambda * (sum(colSums((x1 - X0)^2) * w))
  obj <- loss + penal
  constr <- list(sum(w) == 1, w >= 0)
  prob <- CVXR::Problem(CVXR::Minimize(obj), constr)
  CVXR::psolve(prob, solver = "ECOS")
  .scm_check_status(prob)

  wt_est <- CVXR::value(w)
  y_est <- X00[long_term_col_name, , drop = FALSE] %*% wt_est

  list(wt_est, y_est)
}

#' solver status of a CVXR problem; a seam for tests.
#' @param prob a solved CVXR problem.
#' @return the status string.
#' @noRd
.scm_status <- function(prob) CVXR::status(prob)

#' stop when the solver did not find a solution. "optimal_inaccurate" is
#' accepted: CVXR warns about it, and the solution is usable.
#' @param prob a solved CVXR problem.
#' @return the status, invisibly.
#' @noRd
.scm_check_status <- function(prob) {
  status <- .scm_status(prob)
  if (!status %in% c("optimal", "optimal_inaccurate")) {
    stop("scm() could not solve a matching problem: the solver status is '",
      status, "'. This can happen when covariates have very different ",
      "scales (for example, age in days); rescale them and try again.",
      call. = FALSE
    )
  }
  invisible(status)
}

#' find optimal lambda via leave-one-out cross-validation on EC subjects.
#' for each candidate lambda, holds out one EC subject, constructs SC
#' from remaining ECs, and measures prediction error on OLE outcomes.
#' @param ec attribute matrix for external controls (features x subjects).
#' @param long_term_col_name names of OLE outcome rows.
#' @param lambda_min minimum lambda.
#' @param lambda_max maximum lambda.
#' @param nlambda number of lambda values to evaluate.
#' @return optimal lambda value.
#' @noRd
.scm_lambdacv <- function(ec, long_term_col_name,
                          lambda_min = 0, lambda_max = 0.1, nlambda = 10) {
  lambda_vals <- seq(lambda_min, lambda_max, length.out = nlambda)

  mse_vals <- vapply(lambda_vals, \(lambda) {
    res <- lapply(seq_len(dim(ec)[2]), \(loocv) {
      x1 <- ec[-which(row.names(ec) %in% long_term_col_name), loocv]
      X0 <- ec[-which(row.names(ec) %in% long_term_col_name), -loocv,
        drop = FALSE
      ]

      w <- CVXR::Variable(dim(ec)[2] - 1)
      loss <- sum(((x1 - X0 %*% w))^2)
      penal <- lambda * (sum(colSums((x1 - X0)^2) * w))
      obj <- loss + penal
      constr <- list(sum(w) == 1, w >= 0)
      prob <- CVXR::Problem(CVXR::Minimize(obj), constr)
      CVXR::psolve(prob, solver = "ECOS")
      .scm_check_status(prob)

      wt_est <- CVXR::value(w)
      y_est <- ec[long_term_col_name, -loocv, drop = FALSE] %*% wt_est
      list(wt_est, y_est)
    })
    y_est_mat <- do.call(rbind, lapply(res, \(x) as.vector(x[[2]])))
    mean((ec[long_term_col_name, ] - t(y_est_mat))^2)
  }, numeric(1))

  lambda_vals[which.min(mse_vals)]
}

#' build the attribute matrices for SCM: covariates and all outcomes, with
#' one column per subject, for trial controls and for external controls.
#' @param df internal data frame.
#' @param outcomes outcome column names.
#' @param covariates covariate column names.
#' @return list with X10 (trial controls) and X00 (external controls).
#' @noRd
.scm_matrices <- function(df, outcomes, covariates) {
  S <- df$S
  A <- df$A
  X10 <- t(as.matrix(df[S == 1 & A == 0, c(covariates, outcomes), drop = FALSE]))
  X00 <- t(as.matrix(df[S == 0, c(covariates, outcomes), drop = FALSE]))
  colnames(X10) <- NULL
  colnames(X00) <- NULL
  list(X10 = X10, X00 = X00)
}

#' SCM point estimate (Zhou 2024, Eq 7-9) for a given lambda.
#' @param df internal data frame.
#' @param outcomes outcome column names.
#' @param covariates covariate column names.
#' @param T_cross crossover time point.
#' @param lambda penalty parameter.
#' @return list with tau vector.
#' @noRd
.scm_core <- function(df, outcomes, covariates, T_cross, lambda) {
  # see Zhou 2024: Eq 7-9 (SCM optimization and ATE estimation)
  S <- df$S
  A <- df$A
  long_term_col_name <- outcomes[(T_cross + 1):length(outcomes)]
  mats <- .scm_matrices(df, outcomes, covariates)

  # construct synthetic controls for each RCT control subject
  res <- lapply(seq_len(ncol(mats$X10)), .scm_subject_sc,
    X10 = mats$X10, X00 = mats$X00,
    long_term_col_name = long_term_col_name, lambda = lambda
  )
  y_est_mat <- do.call(rbind, lapply(res, \(x) as.vector(x[[2]])))

  # aggregate: treated OLE outcomes minus synthetic control OLE outcomes
  Y_trt <- colMeans(df[S == 1 & A == 1, long_term_col_name, drop = FALSE])
  list(tau = Y_trt - colMeans(y_est_mat))
}

#' bootstrap statistic for SCM.
#' reconstructs synthetic controls on resampled data with pre-computed lambda.
#' @param data internal data frame.
#' @param indices bootstrap sample indices.
#' @param outcomes outcome column names.
#' @param covariates covariate column names.
#' @param T_cross crossover time point.
#' @param lambda pre-computed penalty parameter.
#' @return numeric vector of tau estimates.
#' @noRd
.scm_boot_statistic <- function(data, indices, outcomes, covariates,
                                T_cross, lambda) {
  d <- data[indices, , drop = FALSE]
  .scm_core(d, outcomes, covariates, T_cross, lambda)$tau
}

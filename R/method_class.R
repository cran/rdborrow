# class unions----
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))

#' Method classes
#'
#' @slot method_name character.
#' @slot bootstrap Number of bootstrap replicates, or NULL.
#' @slot bootstrap_ci_type Bootstrap CI type, or NULL.
#'
#' @keywords internal
.method_obj <- setClass(
  "method_obj",
  slots = c(
    method_name = "character",
    bootstrap = "numericOrNULL",
    bootstrap_ci_type = "characterOrNULL"
  ),
  prototype = list(
    method_name = "",
    bootstrap = NULL,
    bootstrap_ci_type = NULL
  )
)

.method_primary_obj <- setClass(
  "method_primary_obj",
  contains = "method_obj"
)

.method_OLE_obj <- setClass(
  "method_OLE_obj",
  contains = "method_obj"
)

#' Run estimation for a method object
#'
#' S4 generic that dispatches to the appropriate estimation logic
#' based on the method class. Each method subclass (e.g.,
#' \code{ec_ipw_method}, \code{did_ec_ipw_method}) implements its own
#' \code{estimate()} method containing the full estimation pipeline.
#'
#' @param method An S4 method object (e.g., from \code{\link{ec_ipw}}).
#' @param data Data frame with all subjects (RCT + external controls).
#' @param outcomes Character vector of outcome column names.
#' @param treatment Name of the treatment column.
#' @param trial_status Name of the trial participation column.
#' @param covariates Character vector of covariate column names.
#' @param alpha Significance level (default 0.05).
#' @param quiet Logical. Suppress output (default TRUE).
#' @param T_cross Integer crossover time point (OLE methods only).
#' @param ... Additional method-specific arguments.
#'
#' @return A list with estimation results.
#' @export
#'
#' @examples
#' method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
#' estimate(
#'   method,
#'   data = SyntheticData,
#'   outcomes = c("y1", "y2"),
#'   treatment = "A",
#'   trial_status = "S",
#'   covariates = c("x1", "x2", "x3", "x4", "x5")
#' )
setGeneric("estimate", function(method, ...) standardGeneric("estimate"))

#' Run stratified bootstrap and extract confidence intervals.
#' Shared by all method classes that support bootstrap inference.
#' Stratifies by interaction(S, A) to preserve group proportions.
#' @param df data frame with columns S (trial status) and A (treatment).
#' @param statistic function with signature (data, indices, ...) returning
#'   a numeric vector of point estimates.
#' @param n_estimates number of estimates returned by statistic (length of tau).
#' @param bootstrap number of bootstrap replicates.
#' @param bootstrap_ci_type short CI type name ("perc", "bca", etc.).
#' @param alpha significance level for CIs.
#' @param parallel parallelization type for boot ("no", "multicore", "snow").
#' @param ncpus number of CPUs for parallel bootstrap.
#' @param ... additional arguments passed through to statistic.
#' @return list with lower_ci, upper_ci (vectors), and sd_boot (vector).
#' @noRd
.run_bootstrap <- function(df, statistic, n_estimates, bootstrap,
                           bootstrap_ci_type, alpha,
                           parallel = "no", ncpus = 1L, ...) {
  group_id <- as.integer(interaction(df$S, df$A, drop = TRUE))

  ci_type_long <- switch(bootstrap_ci_type,
    norm = "normal",
    bca = "bca",
    stud = "student",
    perc = "percent",
    basic = "basic"
  )

  boot_out <- boot::boot(
    data = df,
    statistic = statistic,
    R = bootstrap,
    strata = group_id,
    parallel = parallel,
    ncpus = ncpus,
    ...
  )

  ci_bounds <- vapply(seq_len(n_estimates), \(i) {
    ci <- boot::boot.ci(boot_out,
      conf = 1 - alpha,
      type = bootstrap_ci_type, index = i
    )
    ci[[ci_type_long]][4:5]
  }, numeric(2))

  sd_boot <- sqrt(diag(var(boot_out$t)))

  list(lower_ci = ci_bounds[1, ], upper_ci = ci_bounds[2, ], sd_boot = sd_boot)
}

#' @noRd
.build_analysis_df <- function(data, outcomes, treatment, trial_status,
                               covariates) {
  Y <- as.matrix(data[, outcomes, drop = FALSE])
  data.frame(Y,
    S = data[[trial_status]], A = data[[treatment]],
    data[, covariates, drop = FALSE]
  )
}

#' Create a base method_obj (internal, used only in tests).
#' @param method_name character identifier for the method.
#' @return a method_obj S4 instance.
#' @noRd
setup_method <- function(method_name = "") {
  checkmate::assert_string(method_name)
  .method_obj(method_name = method_name)
}

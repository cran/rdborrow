#' Analysis OLE class
#'
#' @slot method_obj A `method_OLE_obj` specifying the estimation method.
#' @slot T_cross Numeric crossover time point.
#'
#' @keywords internal
#' @include method_DID_class.R
#' @include method_SCM_class.R
#' @include method_class.R
#' @include analysis_class.R
.analysis_OLE_obj <- setClass(
  "analysis_OLE_obj",
  contains = "analysis_obj",
  slots = c(
    method_obj = "method_OLE_obj",
    T_cross = "numeric"
  )
)

setMethod(
  f = "show",
  signature = "analysis_OLE_obj",
  definition = function(object) {
    cat("<analysis_OLE_obj>\n")
    cat("  Observations:", nrow(object@data), "\n")
    cat("  Trial status:", object@trial_status_col_name, "\n")
    cat("  Treatment:", object@treatment_col_name, "\n")
    cat("  Outcomes:", paste(object@outcome_col_name, collapse = ", "), "\n")
    cat("  Covariates:", paste(object@covariates_col_name, collapse = ", "), "\n")
    cat("  Method:", object@method_obj@method_name, "\n")
    cat("  T_cross:", object@T_cross, "\n")
    cat("  Alpha:", object@alpha, "\n")
  }
)

#' Set up an open-label extension (OLE) analysis
#'
#' Bundles data, column mappings, crossover time, and a method object
#' into an analysis object ready to be passed to \code{\link{run_analysis}}.
#'
#' Available OLE methods:
#' \describe{
#'   \item{\code{\link{did_ec_ipw}}}{Difference-in-differences with IPW.}
#'   \item{\code{\link{did_ec_aipw}}}{DID with augmented IPW.}
#'   \item{\code{\link{did_ec_or}}}{DID with outcome regression.}
#'   \item{\code{\link{scm}}}{Synthetic control method.}
#' }
#'
#' @param data A data frame containing all subject-level data.
#' @param trial_status_col_name Name of the trial status column.
#' @param treatment_col_name Name of the treatment column.
#' @param outcome_col_name Character vector of outcome column names
#'   covering both placebo-controlled and OLE periods.
#' @param covariates_col_name Character vector of covariate column names.
#' @param method_OLE_obj A method object created by
#'   \code{\link{did_ec_ipw}}, \code{\link{did_ec_aipw}},
#'   \code{\link{did_ec_or}}, or \code{\link{scm}}.
#' @param T_cross Integer crossover time point (column index boundary).
#'   The first \code{T_cross} outcome columns are from the
#'   placebo-controlled phase and are used as negative controls for
#'   bias correction. The remaining \code{length(outcome_col_name) -
#'   T_cross} columns are from the open-label extension phase and are
#'   used to estimate the treatment effect. Must be a positive integer
#'   strictly less than \code{length(outcome_col_name)}.
#' @param alpha Significance level (default 0.05).
#'
#' @return An object of class \code{analysis_OLE_obj}, to be passed to
#'   \code{\link{run_analysis}}.
#'
#' @seealso \code{\link{run_analysis}}, \code{\link{setup_analysis_primary}}
#'
#' @export
#'
#' @examples
#' method <- did_ec_ipw(
#'   ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
#'   trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
#'   bootstrap = 50
#' )
#' setup_analysis_OLE(
#'   data = SyntheticData,
#'   trial_status_col_name = "S",
#'   treatment_col_name = "A",
#'   outcome_col_name = c("y1", "y2", "y3", "y4"),
#'   covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
#'   method_OLE_obj = method,
#'   T_cross = 2
#' )
setup_analysis_OLE <- function(data, trial_status_col_name,
                               treatment_col_name,
                               outcome_col_name,
                               covariates_col_name,
                               method_OLE_obj,
                               T_cross, alpha = 0.05) {
  .validate_analysis_base(
    data, trial_status_col_name, treatment_col_name,
    outcome_col_name, covariates_col_name, alpha
  )
  checkmate::assert_class(method_OLE_obj, "method_OLE_obj")
  checkmate::assert_int(T_cross, lower = 1)
  if (T_cross >= length(outcome_col_name)) {
    stop(
      "T_cross must be less than the number of outcomes (got ",
      T_cross, " for ", length(outcome_col_name), " outcomes).",
      call. = FALSE
    )
  }

  .analysis_OLE_obj(
    data = data,
    covariates_col_name = covariates_col_name,
    outcome_col_name = outcome_col_name,
    treatment_col_name = treatment_col_name,
    trial_status_col_name = trial_status_col_name,
    method_obj = method_OLE_obj,
    T_cross = T_cross,
    alpha = alpha
  )
}

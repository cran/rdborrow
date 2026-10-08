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
#'   It must have trial treated patients, trial controls, and external
#'   controls.
#' @param trial_status_col_name Name of the trial status column: 1 for
#'   trial patients, 0 for external controls. Must be numeric or logical.
#' @param treatment_col_name Name of the treatment column: 1 for treated, 0
#'   for control. Must be numeric or logical, not a factor. External controls
#'   must have 0.
#' @param outcome_col_name Character vector of outcome column names
#'   covering both placebo-controlled and OLE periods, in visit order. The
#'   position of each outcome sets its period and its result row name. The columns must have
#'   no missing values.
#' @param covariates_col_name Character vector of covariate column names. The
#'   columns must have no missing values.
#'   Outcome and covariate columns cannot be named \code{S} or \code{A}, or
#'   be the trial-status or treatment column.
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
#' @param alpha Significance level, more than 0 and less than 1 (default
#'   0.05).
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
  T_cross <- .check_T_cross(T_cross, outcome_col_name)

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

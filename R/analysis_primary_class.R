#' Analysis primary class
#'
#' @slot method_obj A `method_primary_obj` specifying the estimation method.
#'
#' @keywords internal
#' @include analysis_class.R
#' @include method_weighting_class.R
.analysis_primary_obj <- setClass(
  "analysis_primary_obj",
  contains = "analysis_obj",
  slots = c(
    method_obj = "method_primary_obj"
  )
)

setMethod(
  f = "show",
  signature = "analysis_primary_obj",
  definition = function(object) {
    cat("<analysis_primary_obj>\n")
    cat("  Observations:", nrow(object@data), "\n")
    cat("  Trial status:", object@trial_status_col_name, "\n")
    cat("  Treatment:", object@treatment_col_name, "\n")
    cat("  Outcomes:", paste(object@outcome_col_name, collapse = ", "), "\n")
    cat("  Covariates:", paste(object@covariates_col_name, collapse = ", "), "\n")
    cat("  Method:", object@method_obj@method_name, "\n")
    cat("  Alpha:", object@alpha, "\n")
  }
)

#' Set up a primary analysis with external control borrowing
#'
#' Bundles data, column mappings, and a method object into an analysis
#' object ready to be passed to \code{\link{run_analysis}}.
#'
#' Available primary methods:
#' \describe{
#'   \item{\code{\link{ec_ipw}}}{Inverse probability weighting.}
#'   \item{\code{\link{ec_aipw}}}{Augmented inverse probability weighting
#'     (doubly robust).}
#' }
#'
#' @param data A data frame containing all subject-level data.
#'   It must have trial treated patients and trial controls. It must also
#'   have external controls, unless the method is \code{ec_ipw(weight = 0)}.
#' @param trial_status_col_name Name of the trial status column: 1 for
#'   trial patients, 0 for external controls. Must be numeric or logical.
#' @param treatment_col_name Name of the treatment column: 1 for treated, 0
#'   for control. Must be numeric or logical, not a factor. External controls
#'   must have 0.
#' @param outcome_col_name Character vector of outcome column names. The
#'   columns must have no missing values.
#' @param covariates_col_name Character vector of covariate column names. The
#'   columns must have no missing values.
#'   Outcome and covariate columns cannot be named \code{S} or \code{A}, or
#'   be the trial-status or treatment column.
#' @param method_weighting_obj A method object created by
#'   \code{\link{ec_ipw}} or \code{\link{ec_aipw}}.
#' @param alpha Significance level, more than 0 and less than 1 (default
#'   0.05).
#'
#' @return An object of class \code{analysis_primary_obj}, to be passed to
#'   \code{\link{run_analysis}}.
#'
#' @seealso \code{\link{run_analysis}}, \code{\link{setup_analysis_OLE}}
#'
#' @export
#'
#' @examples
#' method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
#' setup_analysis_primary(
#'   data = SyntheticData,
#'   trial_status_col_name = "S",
#'   treatment_col_name = "A",
#'   outcome_col_name = c("y1", "y2"),
#'   covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
#'   method_weighting_obj = method
#' )
setup_analysis_primary <- function(data, trial_status_col_name, treatment_col_name,
                                   outcome_col_name, covariates_col_name, method_weighting_obj,
                                   alpha = 0.05) {
  .validate_analysis_base(
    data, trial_status_col_name, treatment_col_name,
    outcome_col_name, covariates_col_name, alpha,
    external = FALSE
  )
  checkmate::assert_class(method_weighting_obj, "method_primary_obj")

  .analysis_primary_obj(
    data = data,
    covariates_col_name = covariates_col_name,
    outcome_col_name = outcome_col_name,
    treatment_col_name = treatment_col_name,
    trial_status_col_name = trial_status_col_name,
    method_obj = method_weighting_obj,
    alpha = alpha
  )
}

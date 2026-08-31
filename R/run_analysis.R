#' Run an analysis with external control borrowing
#'
#' Estimates treatment effects by combining randomized trial data with
#' external controls. Choose a method, wrap it in an analysis object,
#' and pass it here.
#'
#' Six borrowing methods are available:
#' \describe{
#'   \item{\code{\link{ec_ipw}}}{Inverse probability weighting (primary
#'     analysis).}
#'   \item{\code{\link{ec_aipw}}}{Augmented inverse probability weighting
#'     (primary analysis).}
#'   \item{\code{\link{did_ec_ipw}}}{Difference-in-differences with IPW
#'     (open-label extension).}
#'   \item{\code{\link{did_ec_aipw}}}{Difference-in-differences with AIPW
#'     (open-label extension).}
#'   \item{\code{\link{did_ec_or}}}{Difference-in-differences with outcome
#'     regression (open-label extension).}
#'   \item{\code{\link{scm}}}{Synthetic control method (open-label
#'     extension).}
#' }
#'
#' @param analysis_obj An analysis object created by
#'   \code{\link{setup_analysis_primary}} or \code{\link{setup_analysis_OLE}}.
#' @param quiet Logical. If \code{TRUE}, suppress printed output.
#'
#' @return For primary methods, a list with \code{results} (data frame of
#'   point estimates, standard errors, and confidence intervals) and
#'   \code{borrow_weight}. For OLE methods, a data frame of point estimates
#'   and bootstrap confidence intervals.
#'
#' @seealso \code{\link{run_simulation}} for evaluating operating
#'   characteristics via Monte Carlo simulation.
#'
#' @export
#'
#' @examples
#' method <- ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
#' analysis <- setup_analysis_primary(
#'   data = SyntheticData,
#'   trial_status_col_name = "S",
#'   treatment_col_name = "A",
#'   outcome_col_name = c("y1", "y2"),
#'   covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
#'   method_weighting_obj = method
#' )
#' run_analysis(analysis)
run_analysis <- function(analysis_obj, quiet = TRUE) {
  method <- analysis_obj@method_obj

  args <- list(
    method = method,
    data = analysis_obj@data,
    outcomes = analysis_obj@outcome_col_name,
    treatment = analysis_obj@treatment_col_name,
    trial_status = analysis_obj@trial_status_col_name,
    covariates = analysis_obj@covariates_col_name,
    alpha = analysis_obj@alpha,
    quiet = quiet
  )
  if (is(analysis_obj, "analysis_OLE_obj")) {
    args$T_cross <- analysis_obj@T_cross
  }

  do.call(estimate, args)
}

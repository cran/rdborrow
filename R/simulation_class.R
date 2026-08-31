#' Simulation class
#'
#' @slot covariates_col_name Character vector of covariate column names.
#' @slot outcome_col_name Character vector of outcome column names.
#' @slot treatment_col_name Name of the treatment column.
#' @slot trial_status_col_name Name of the trial status column.
#' @slot alpha Significance level.
#' @slot method_obj_list List of method objects to evaluate.
#' @slot method_description Character vector of method labels.
#'
#' @keywords internal
#' @include method_class.R
.simulation_obj <- setClass(
  "simulation_obj",
  slots = c(
    covariates_col_name = "character",
    outcome_col_name = "character",
    treatment_col_name = "character",
    trial_status_col_name = "character",
    method_obj_list = "list",
    alpha = "numeric",
    method_description = "character"
  ),
  prototype = list(
    covariates_col_name = ""
  )
)

#' Simulation for primary analysis
#'
#' @slot data_matrix_list_null List of data frames simulated under the null.
#' @slot data_matrix_list_alt List of data frames simulated under the alternative.
#' @slot true_effect Numeric vector of true treatment effects.
#' @slot alt_effect Numeric vector of alternative treatment effects.
#'
#' @keywords internal
.simulation_primary_obj <- setClass(
  "simulation_primary_obj",
  contains = "simulation_obj",
  slots = c(
    data_matrix_list_null = "list",
    data_matrix_list_alt = "list",
    true_effect = "numeric",
    alt_effect = "numeric"
  ),
  prototype = list(
    data_matrix_list_alt = list(),
    alt_effect = numeric(0)
  )
)

#' Simulation for OLE study
#'
#' @slot data_matrix_list List of simulated data matrices.
#' @slot true_effect True treatment effect for evaluating estimator performance.
#' @slot T_cross Numeric crossover time point for the OLE phase.
#'
#' @keywords internal
.simulation_OLE_obj <- setClass(
  "simulation_OLE_obj",
  contains = "simulation_obj",
  slots = c(
    data_matrix_list = "list",
    true_effect = "numeric",
    T_cross = "numeric"
  )
)

setMethod(
  f = "show",
  signature = "simulation_obj",
  definition = function(object) {
    cat("<simulation_obj>\n")
    cat("  Trial status:", object@trial_status_col_name, "\n")
    cat("  Treatment:", object@treatment_col_name, "\n")
    cat("  Outcomes:", paste(object@outcome_col_name, collapse = ", "), "\n")
    cat("  Covariates:", paste(object@covariates_col_name, collapse = ", "), "\n")
    cat("  Methods:", paste(object@method_description, collapse = ", "), "\n")
    cat("  Alpha:", object@alpha, "\n")
  }
)

.validate_simulation_base <- function(trial_status_col_name, treatment_col_name,
                                      outcome_col_name, covariates_col_name,
                                      method_obj_list, method_description, alpha) {
  checkmate::assert_string(trial_status_col_name)
  checkmate::assert_string(treatment_col_name)
  checkmate::assert_character(outcome_col_name, min.len = 1)
  checkmate::assert_character(covariates_col_name, min.len = 1)
  checkmate::assert_list(method_obj_list, min.len = 1)
  for (i in seq_along(method_obj_list)) {
    checkmate::assert_class(method_obj_list[[i]], "method_obj")
  }
  checkmate::assert_character(method_description, len = length(method_obj_list))
  checkmate::assert_number(alpha, lower = 0, upper = 1)
}

#' @noRd
setup_simulation <- function(trial_status_col_name,
                             treatment_col_name,
                             outcome_col_name,
                             covariates_col_name,
                             method_obj_list,
                             method_description,
                             alpha = 0.05) {
  .validate_simulation_base(
    trial_status_col_name, treatment_col_name, outcome_col_name,
    covariates_col_name, method_obj_list, method_description, alpha
  )

  simulation_obj <- .simulation_obj(
    covariates_col_name = covariates_col_name,
    outcome_col_name = outcome_col_name,
    treatment_col_name = treatment_col_name,
    trial_status_col_name = trial_status_col_name,
    method_obj_list = method_obj_list,
    alpha = alpha,
    method_description = method_description
  )
}

#' Construct a simulation object for primary analysis
#'
#' @param data_matrix_list_null List of data frames simulated under the null.
#' @param trial_status_col_name Name of the trial status column.
#' @param treatment_col_name Name of the treatment column.
#' @param outcome_col_name Character vector of outcome column names.
#' @param covariates_col_name Character vector of covariate column names.
#' @param method_obj_list List of method objects to evaluate.
#' @param true_effect Numeric vector of true treatment effects.
#' @param method_description Character vector of method labels, one per method
#'   in `method_obj_list`.
#' @param data_matrix_list_alt List of data frames simulated under the
#'   alternative.
#' @param alt_effect Numeric vector of alternative treatment effects.
#' @param alpha Significance level.
#'
#' @return An object of class `simulation_primary_obj`.
#' @export
#'
#' @examples
#' setup_simulation_primary(
#'   data_matrix_list_null = list(SyntheticData),
#'   trial_status_col_name = "S",
#'   treatment_col_name = "A",
#'   outcome_col_name = c("y1", "y2"),
#'   covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
#'   method_obj_list = list(
#'     ec_ipw(ps_formula = "S ~ x1 + x2 + x3 + x4 + x5")
#'   ),
#'   true_effect = c(0, 0),
#'   method_description = "IPW"
#' )
setup_simulation_primary <- function(data_matrix_list_null,
                                     trial_status_col_name,
                                     treatment_col_name,
                                     outcome_col_name,
                                     covariates_col_name,
                                     method_obj_list,
                                     true_effect,
                                     method_description,
                                     data_matrix_list_alt = list(),
                                     alt_effect = numeric(0),
                                     alpha = 0.05) {
  .validate_simulation_base(
    trial_status_col_name, treatment_col_name, outcome_col_name,
    covariates_col_name, method_obj_list, method_description, alpha
  )
  checkmate::assert_list(data_matrix_list_null, min.len = 1)
  for (i in seq_along(data_matrix_list_null)) {
    checkmate::assert_data_frame(data_matrix_list_null[[i]])
  }
  checkmate::assert_numeric(true_effect, min.len = 1)
  checkmate::assert_list(data_matrix_list_alt)
  for (i in seq_along(data_matrix_list_alt)) {
    checkmate::assert_data_frame(data_matrix_list_alt[[i]])
  }
  checkmate::assert_numeric(alt_effect)

  simulation_primary_obj <- .simulation_primary_obj(
    data_matrix_list_null = data_matrix_list_null,
    data_matrix_list_alt = data_matrix_list_alt,
    covariates_col_name = covariates_col_name,
    outcome_col_name = outcome_col_name,
    treatment_col_name = treatment_col_name,
    trial_status_col_name = trial_status_col_name,
    method_obj_list = method_obj_list,
    alpha = alpha,
    true_effect = true_effect,
    alt_effect = alt_effect,
    method_description = method_description
  )
}

#' Construct a simulation object for OLE analysis
#'
#' @param data_matrix_list List of simulated data frames.
#' @param trial_status_col_name Name of the trial status column.
#' @param treatment_col_name Name of the treatment column.
#' @param outcome_col_name Character vector of outcome column names.
#' @param covariates_col_name Character vector of covariate column names.
#' @param method_obj_list List of method objects to evaluate.
#' @param T_cross Numeric crossover time point.
#' @param true_effect Numeric vector of true treatment effects.
#' @param method_description Character vector of method labels, one per method
#'   in `method_obj_list`.
#' @param alpha Significance level.
#'
#' @return An object of class `simulation_OLE_obj`.
#' @export
#'
#' @examples
#' setup_simulation_OLE(
#'   data_matrix_list = list(SyntheticData),
#'   trial_status_col_name = "S",
#'   treatment_col_name = "A",
#'   outcome_col_name = c("y1", "y2", "y3", "y4"),
#'   covariates_col_name = c("x1", "x2", "x3", "x4", "x5"),
#'   method_obj_list = list(
#'     did_ec_ipw(
#'       ps_formula = "S ~ x1 + x2 + x3 + x4 + x5",
#'       trt_formula = "A ~ x1 + x2 + x3 + x4 + x5",
#'       bootstrap = 50
#'     )
#'   ),
#'   T_cross = 2,
#'   true_effect = c(0, 0),
#'   method_description = "IPW, DID"
#' )
setup_simulation_OLE <- function(data_matrix_list,
                                 trial_status_col_name,
                                 treatment_col_name,
                                 outcome_col_name,
                                 covariates_col_name,
                                 method_obj_list,
                                 T_cross,
                                 true_effect,
                                 method_description,
                                 alpha = 0.05) {
  .validate_simulation_base(
    trial_status_col_name, treatment_col_name, outcome_col_name,
    covariates_col_name, method_obj_list, method_description, alpha
  )
  checkmate::assert_list(data_matrix_list, min.len = 1)
  for (i in seq_along(data_matrix_list)) {
    checkmate::assert_data_frame(data_matrix_list[[i]])
  }
  checkmate::assert_number(T_cross, lower = 0)
  checkmate::assert_numeric(true_effect, min.len = 1)

  simulation_OLE_obj <- .simulation_OLE_obj(
    data_matrix_list = data_matrix_list,
    covariates_col_name = covariates_col_name,
    outcome_col_name = outcome_col_name,
    treatment_col_name = treatment_col_name,
    trial_status_col_name = trial_status_col_name,
    method_obj_list = method_obj_list,
    alpha = alpha,
    T_cross = T_cross,
    true_effect = true_effect,
    method_description = method_description
  )
}

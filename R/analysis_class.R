#' Analysis class
#'
#' @slot method_obj Method.
#' @slot data Data frame of subject-level data.
#' @slot covariates_col_name Character vector of covariate column names.
#' @slot outcome_col_name Character vector of outcome column names.
#' @slot treatment_col_name Name of the treatment column.
#' @slot trial_status_col_name Name of the trial status column.
#' @slot alpha Significance level.
#'
#' @keywords internal
#' @include method_class.R
.analysis_obj <- setClass(
  "analysis_obj",
  slots = c(
    data = "data.frame",
    covariates_col_name = "character",
    outcome_col_name = "character",
    treatment_col_name = "character",
    trial_status_col_name = "character",
    method_obj = "method_obj",
    alpha = "numeric"
  ),
  prototype = list(
    covariates_col_name = ""
  )
)

setMethod(
  f = "show",
  signature = "analysis_obj",
  definition = function(object) {
    cat("<analysis_obj>\n")
    cat("  Observations:", nrow(object@data), "\n")
    cat("  Trial status:", object@trial_status_col_name, "\n")
    cat("  Treatment:", object@treatment_col_name, "\n")
    cat("  Outcomes:", paste(object@outcome_col_name, collapse = ", "), "\n")
    cat("  Covariates:", paste(object@covariates_col_name, collapse = ", "), "\n")
    cat("  Method:", object@method_obj@method_name, "\n")
    cat("  Alpha:", object@alpha, "\n")
  }
)

.validate_analysis_base <- function(data, trial_status_col_name,
                                    treatment_col_name, outcome_col_name,
                                    covariates_col_name, alpha,
                                    external = TRUE) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(trial_status_col_name)
  checkmate::assert_string(treatment_col_name)
  checkmate::assert_character(outcome_col_name, min.len = 1)
  checkmate::assert_character(covariates_col_name, min.len = 1)
  .check_alpha(alpha)
  checkmate::assert_subset(
    c(
      trial_status_col_name, treatment_col_name,
      outcome_col_name, covariates_col_name
    ),
    choices = names(data)
  )
  .check_internal_names(
    outcome_col_name, covariates_col_name,
    trial_status_col_name, treatment_col_name
  )

  .check_status_and_treatment(
    data, trial_status_col_name, treatment_col_name, external
  )
  checkmate::assert_data_frame(
    data[c(outcome_col_name, covariates_col_name)],
    any.missing = FALSE, .var.name = "data"
  )
}

setup_analysis <- function(data, trial_status_col_name, treatment_col_name,
                           outcome_col_name, covariates_col_name, method_obj,
                           alpha = 0.05) {
  .validate_analysis_base(
    data, trial_status_col_name, treatment_col_name,
    outcome_col_name, covariates_col_name, alpha
  )
  checkmate::assert_class(method_obj, "method_obj")

  analysis_obj <- .analysis_obj(
    data = data,
    covariates_col_name = covariates_col_name,
    outcome_col_name = outcome_col_name,
    treatment_col_name = treatment_col_name,
    trial_status_col_name = trial_status_col_name,
    method_obj = method_obj,
    alpha = alpha
  )
}

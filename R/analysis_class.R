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
                                    covariates_col_name, alpha) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(trial_status_col_name)
  checkmate::assert_string(treatment_col_name)
  checkmate::assert_character(outcome_col_name, min.len = 1)
  checkmate::assert_character(covariates_col_name, min.len = 1)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_subset(
    c(
      trial_status_col_name, treatment_col_name,
      outcome_col_name, covariates_col_name
    ),
    choices = names(data)
  )

  # S and A must be binary 0/1
  if (!all(data[[trial_status_col_name]] %in% c(0L, 1L, 0, 1))) {
    stop("Column '", trial_status_col_name, "' must contain only 0 and 1.",
      call. = FALSE
    )
  }
  if (!all(data[[treatment_col_name]] %in% c(0L, 1L, 0, 1))) {
    stop("Column '", treatment_col_name, "' must contain only 0 and 1.",
      call. = FALSE
    )
  }
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

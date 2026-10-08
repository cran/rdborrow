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
#' @param alpha Significance level, more than 0 and less than 1 (default
#'   0.05).
#' @param quiet Logical. Suppress output (default TRUE).
#' @param T_cross Integer crossover time point (OLE methods only).
#' @param ... Additional method-specific arguments.
#'
#' @return For primary methods, a list with \code{results} and
#'   \code{borrow_weight}; for OLE methods, a data frame. See
#'   \code{\link{run_analysis}} for the columns and row names.
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
#' @param bootstrap_ci_type short CI type name ("perc", "bca", "norm", "basic").
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
    perc = "percent",
    basic = "basic"
  )

  # capture the extra arguments so boot sees a plain (data, indices)
  # statistic. boot.ci(type = "bca") re-invokes it through empinf(), which
  # does not forward boot's ... and would otherwise error.
  dots <- list(...)
  stat_fn <- function(data, indices) {
    do.call(statistic, c(list(data, indices), dots))
  }

  boot_out <- boot::boot(
    data = df,
    statistic = stat_fn,
    R = bootstrap,
    strata = group_id,
    parallel = parallel,
    ncpus = ncpus
  )

  ci_bounds <- vapply(seq_len(n_estimates), \(i) {
    ci <- boot::boot.ci(boot_out,
      conf = 1 - alpha,
      type = bootstrap_ci_type, index = i
    )
    # the normal component is a 3-column matrix, the others are 5-column
    bounds <- if (ci_type_long == "normal") 2:3 else 4:5
    ci[[ci_type_long]][bounds]
  }, numeric(2))

  sd_boot <- sqrt(diag(var(boot_out$t)))

  list(lower_ci = ci_bounds[1, ], upper_ci = ci_bounds[2, ], sd_boot = sd_boot)
}

#' @noRd
.build_analysis_df <- function(data, outcomes, treatment, trial_status,
                               covariates, external = TRUE) {
  .check_internal_names(outcomes, covariates, trial_status, treatment)
  .check_status_and_treatment(data, trial_status, treatment, external)
  checkmate::assert_data_frame(
    data[c(outcomes, covariates)],
    any.missing = FALSE, .var.name = "data"
  )
  Y <- as.matrix(data[, outcomes, drop = FALSE])
  data.frame(Y,
    S = data[[trial_status]], A = data[[treatment]],
    data[, covariates, drop = FALSE],
    check.names = FALSE
  )
}

#' Check the crossover time and return it as an exact whole number.
#' checkmate::assert_int() accepts values within a tolerance of an integer,
#' such as 0.6 / 0.2, but `:` and indexing then use the inexact value.
#' @param T_cross crossover time point.
#' @param outcomes outcome column names.
#' @return `T_cross`, rounded to a whole number.
#' @noRd
.check_T_cross <- function(T_cross, outcomes) {
  checkmate::assert_int(T_cross, lower = 1)
  T_cross <- round(T_cross)
  if (T_cross >= length(outcomes)) {
    stop(
      "T_cross must be less than the number of outcomes (got ",
      T_cross, " for ", length(outcomes), " outcomes).",
      call. = FALSE
    )
  }
  T_cross
}

#' Check that no outcome or covariate name repeats, and that none is named S
#' or A, the internal names of the trial-status and treatment columns, or is
#' one of those columns. The methods would otherwise read the wrong column.
#' @param outcomes outcome column names.
#' @param covariates covariate column names.
#' @param trial_status trial-status column name.
#' @param treatment treatment column name.
#' @return `NULL`, invisibly.
#' @noRd
.check_internal_names <- function(outcomes, covariates, trial_status,
                                  treatment) {
  all_names <- c(outcomes, covariates)
  repeated <- unique(all_names[duplicated(all_names)])
  if (length(repeated) > 0) {
    stop("Each outcome and covariate column can be listed only once, as an ",
      "outcome or as a covariate; repeated: ", paste(repeated, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  clash <- intersect(
    c(outcomes, covariates), c("S", "A", trial_status, treatment)
  )
  if (length(clash) > 0) {
    stop("Outcome and covariate columns cannot use the names S and A, which ",
      "the package uses internally, or be the trial-status or treatment ",
      "column. Rename or remove: ", paste(clash, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Check that a trial-status or treatment column is numeric or logical and
#' holds only 0 and 1. %in% compares a factor's labels, so a factor with
#' levels c("1", "0") would pass a value check alone.
#' @param x the column.
#' @param col the column name, for error messages.
#' @return `x`, invisibly.
#' @noRd
.check_binary_column <- function(x, col) {
  checkmate::assert(
    checkmate::check_numeric(x),
    checkmate::check_logical(x),
    .var.name = col
  )
  if (!all(x %in% c(0, 1))) {
    stop("Column '", col, "' must contain only 0 and 1.", call. = FALSE)
  }
  invisible(x)
}

#' Check that the significance level is strictly between 0 and 1.
#' checkmate::assert_number() has inclusive bounds only, and alpha = 0 or 1
#' gives infinite, missing, or zero-width intervals.
#' @param alpha significance level.
#' @return `alpha`, invisibly.
#' @noRd
.check_alpha <- function(alpha) {
  checkmate::assert_number(alpha)
  if (alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be between 0 and 1, not ", alpha, ".", call. = FALSE)
  }
  invisible(alpha)
}

#' Check the trial-status and treatment columns: each holds only 0 and 1,
#' every external control (trial status 0) has treatment 0, and no group
#' that the method uses is empty.
#' @param data data frame.
#' @param trial_status trial-status column name.
#' @param treatment treatment column name.
#' @param external whether the data must have external controls.
#' @return `data`, invisibly.
#' @noRd
.check_status_and_treatment <- function(data, trial_status, treatment,
                                        external = TRUE) {
  .check_binary_column(data[[trial_status]], trial_status)
  .check_binary_column(data[[treatment]], treatment)
  n_treated_ext <- sum(data[[trial_status]] == 0 & data[[treatment]] == 1)
  if (n_treated_ext > 0) {
    stop("External controls must have treatment 0, but ", n_treated_ext,
      " external controls have treatment 1 (column '", treatment, "' is 1 ",
      "where column '", trial_status, "' is 0).",
      call. = FALSE
    )
  }
  S <- data[[trial_status]]
  A <- data[[treatment]]
  groups <- c(
    "trial treated patients (trial status 1, treatment 1)" = sum(S == 1 & A == 1),
    "trial controls (trial status 1, treatment 0)" = sum(S == 1 & A == 0),
    "external controls (trial status 0)" = if (external) sum(S == 0) else NA
  )
  empty <- names(groups)[groups %in% 0]
  if (length(empty) > 0) {
    stop("The data has no ", paste(empty, collapse = " and no "), ".",
      call. = FALSE
    )
  }
  invisible(data)
}

#' Check that the right-hand side of each formula uses only covariates. The
#' internal data also holds the outcomes, S and A, which `.` would add, and
#' R looks for any other variable in the formula's environment, which can be
#' the user's R session. Both are errors. An outcome used as a predictor is a
#' warning: it is measured after randomization, so adjusting for it can bias
#' the treatment effect, but the user named it on purpose.
#' @param formulas character vector of formulas, or NULL.
#' @param covariates covariate column names.
#' @param outcomes outcome column names.
#' @param arg argument name, for messages.
#' @return `formulas`, invisibly.
#' @noRd
.check_formula_covariates <- function(formulas, covariates, outcomes, arg) {
  rhs_outcomes <- character(0)
  for (f in formulas) {
    parsed <- as.formula(f)
    rhs_vars <- all.vars(parsed[[length(parsed)]])
    if ("." %in% rhs_vars) {
      stop("`", arg, "` uses `.`, which would also add the outcomes and the ",
        "treatment to the model. List the covariates instead; got `", f, "`.",
        call. = FALSE
      )
    }
    unknown <- setdiff(rhs_vars, c(covariates, outcomes))
    if (length(unknown) > 0) {
      stop("`", arg, "` uses variables that are not covariates: ",
        paste(unknown, collapse = ", "), ". Add them to ",
        "`covariates_col_name`, or remove them from the formula.",
        call. = FALSE
      )
    }
    rhs_outcomes <- union(rhs_outcomes, intersect(rhs_vars, outcomes))
  }
  if (length(rhs_outcomes) > 0) {
    warning("`", arg, "` uses outcomes as predictors: ",
      paste(rhs_outcomes, collapse = ", "), ". These are measured after ",
      "randomization, so adjusting for them can bias the treatment effect. ",
      "Use baseline covariates only unless this is intended.",
      call. = FALSE
    )
  }
  invisible(formulas)
}

#' Put outcome formulas in the order of the outcomes, matching each formula
#' to an outcome by the outcome name on its left-hand side. A transformed
#' left side such as log(y1) is rejected: the methods would mix its scale
#' with the raw outcome.
#' @param formulas character vector of outcome formulas.
#' @param outcomes outcome column names, in analysis order.
#' @param arg argument name, for error messages.
#' @return `formulas`, reordered to match `outcomes`.
#' @noRd
.match_outcome_formulas <- function(formulas, outcomes, arg) {
  lhs <- vapply(formulas, \(f) {
    parsed <- as.formula(f)
    if (length(parsed) != 3) {
      stop("Each formula in `", arg, "` must have an outcome on its ",
        "left-hand side; got `", f, "`.",
        call. = FALSE
      )
    }
    if (!is.name(parsed[[2]])) {
      stop("The left-hand side of each formula in `", arg, "` must be an ",
        "outcome name; got `", deparse(parsed[[2]]), "`. Transform the ",
        "outcome column before the analysis instead.",
        call. = FALSE
      )
    }
    as.character(parsed[[2]])
  }, character(1), USE.NAMES = FALSE)

  unknown <- setdiff(lhs, outcomes)
  if (length(unknown) > 0) {
    stop("`", arg, "` has formulas for variables that are not outcomes: ",
      paste(unknown, collapse = ", "), ".",
      call. = FALSE
    )
  }
  repeated <- unique(lhs[duplicated(lhs)])
  if (length(repeated) > 0) {
    stop("`", arg, "` has more than one formula for: ",
      paste(repeated, collapse = ", "), ".",
      call. = FALSE
    )
  }
  missing <- setdiff(outcomes, lhs)
  if (length(missing) > 0) {
    stop("`", arg, "` has no formula for: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  formulas[match(outcomes, lhs)]
}

#' Create a base method_obj (internal, used only in tests).
#' @param method_name character identifier for the method.
#' @return a method_obj S4 instance.
#' @noRd
setup_method <- function(method_name = "") {
  checkmate::assert_string(method_name)
  .method_obj(method_name = method_name)
}

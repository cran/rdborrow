#' Synthetic example dataset for rdborrow
#'
#' A simulated dataset containing covariates, treatment, trial status,
#' and longitudinal outcomes for demonstrating external control borrowing methods.
#'
#' @format A data frame with 300 rows and 12 columns: 100 trial patients on
#'   treatment (\code{S = 1}, \code{A = 1}), 100 trial controls
#'   (\code{S = 1}, \code{A = 0}), and 100 external controls (\code{S = 0},
#'   \code{A = 0}). The covariates mimic a spinal muscular atrophy (SMA)
#'   study.
#'   \describe{
#'     \item{x1}{SMA type: 0 = type II, 1 = type III.}
#'     \item{x2}{SMN2 copy number: 0 = 3 copies, 1 = 4 copies.}
#'     \item{x3}{Scoliosis: 0 = no, 1 = yes.}
#'     \item{x4}{Age at enrollment, in years.}
#'     \item{x5}{Baseline outcome.}
#'     \item{A}{Treatment: 1 = treated, 0 = control.}
#'     \item{S}{Trial status: 1 = trial patient, 0 = external control.}
#'     \item{T_cross}{Last visit before trial controls cross over to
#'       treatment: 2 for all rows.}
#'     \item{y1, y2, y3, y4}{Outcomes at visits 1 to 4.}
#'   }
#' @source Simulated with \code{\link{simulate_trial}()}. The script is
#'   \code{data-raw/SyntheticData.R} in the package source on GitHub.
#' @usage data(SyntheticData)
"SyntheticData"

#' Unbalanced synthetic example dataset for rdborrow
#'
#' A companion to \code{\link{SyntheticData}} with unequal group sizes, for
#' tests and examples where balanced data would hide an error. With equal
#' arms, for example, exchanging the randomization probability and its
#' complement gives the same numbers.
#'
#' @format A data frame with 380 rows and the same 12 columns as
#'   \code{\link{SyntheticData}}: 160 trial patients on treatment
#'   (\code{S = 1}, \code{A = 1}), 80 trial controls (\code{S = 1},
#'   \code{A = 0}), and 140 external controls (\code{S = 0}, \code{A = 0}).
#'   The trial randomizes 2:1, so the probability of treatment is 2/3. SMA
#'   type III (\code{x1 = 1}) is more common in the trial than in the
#'   external controls. The participation model has good overlap. The
#'   outcome models are the same as in \code{SyntheticData}, and
#'   \code{T_cross} is 2 for all rows.
#' @source Simulated with \code{\link{simulate_trial}()}. The script is
#'   \code{data-raw/SyntheticDataII.R} in the package source on GitHub.
#' @usage data(SyntheticDataII)
"SyntheticDataII"

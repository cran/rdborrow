#' @import checkmate
#' @importFrom copula mvdc rMvdc normalCopula
#' @import futile.logger
#' @import mvtnorm
#' @import boot
#' @importFrom CVXR Variable Minimize Problem
#' @import progress
#' @importFrom methods is new
#' @importFrom stats as.formula glm lm model.matrix predict qnorm rbinom rmultinom var
#' @importFrom utils data
#' @importFrom Matrix bdiag
NULL

utils::globalVariables(c(".", "rx", "piA", "piS", "piSX"))

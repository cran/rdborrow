#' Setup method weighting (Deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been removed. Use [ec_ipw()] for inverse
#' probability weighting or [ec_aipw()] for augmented inverse
#' probability weighting.
#'
#' @param ... Ignored.
#'
#' @return This function is defunct and always signals an error;
#'   it does not return a value.
#'
#' @export
#'
#' @examples
#' try(setup_method_weighting())
setup_method_weighting <- function(...) {
  .Deprecated(msg = paste(
    "'setup_method_weighting' has been removed.",
    "Use ec_ipw() or ec_aipw() instead."
  ))
  stop(
    "setup_method_weighting() is no longer functional. ",
    "Use ec_ipw() or ec_aipw().",
    call. = FALSE
  )
}

#' Setup method DID (Deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been removed. Use [did_ec_ipw()] for
#' inverse probability weighting, [did_ec_aipw()] for augmented
#' inverse probability weighting, or [did_ec_or()] for outcome
#' regression.
#'
#' @param ... Ignored.
#'
#' @return This function is defunct and always signals an error;
#'   it does not return a value.
#'
#' @export
#'
#' @examples
#' try(setup_method_DID())
setup_method_DID <- function(...) {
  .Deprecated(msg = paste(
    "'setup_method_DID' has been removed.",
    "Use did_ec_ipw(), did_ec_aipw(), or did_ec_or() instead."
  ))
  stop(
    "setup_method_DID() is no longer functional. ",
    "Use did_ec_ipw(), did_ec_aipw(), or did_ec_or().",
    call. = FALSE
  )
}

#' Setup method SCM (Deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been removed. Use [scm()] instead.
#'
#' @param ... Ignored.
#'
#' @return This function is defunct and always signals an error;
#'   it does not return a value.
#'
#' @export
#'
#' @examples
#' try(setup_method_SCM())
setup_method_SCM <- function(...) {
  .Deprecated(msg = paste(
    "'setup_method_SCM' has been removed.",
    "Use scm() instead."
  ))
  stop(
    "setup_method_SCM() is no longer functional. ",
    "Use scm().",
    call. = FALSE
  )
}

#' Setup bootstrap (Deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Bootstrap settings are now specified directly in each method
#' constructor (e.g., [ec_ipw()], [did_ec_ipw()]). The separate
#' bootstrap object is no longer needed.
#'
#' @param ... Ignored.
#'
#' @return This function is defunct and always signals an error;
#'   it does not return a value.
#'
#' @export
#'
#' @examples
#' try(setup_bootstrap())
setup_bootstrap <- function(...) {
  .Deprecated(msg = paste(
    "'setup_bootstrap' has been removed.",
    "Bootstrap settings are now passed directly to method constructors",
    "(e.g., ec_ipw(bootstrap = 500, bootstrap_ci_type = 'perc'))."
  ))
  stop(
    "setup_bootstrap() is no longer functional. ",
    "Pass bootstrap settings directly to method constructors.",
    call. = FALSE
  )
}

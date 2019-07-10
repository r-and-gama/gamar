# obs_rates ------------------------------------------------------------------
#' Extract monitoring rates
#'
#' Subsets the columns of an \code{experiment} object that correspond to the
#' obs_rates rates of the monitored variables.
#'
#' @param exp An object of class \code{experiment}.
#'
#' @return A data frame that is a subset of the inputed \code{experiment} object.
#'
#' @example inst/examples/obs_rates.R
#' @export
obs_rates <- function(exp) UseMethod("obs_rates")

#' @rdname obs_rates
#' @export
obs_rates.default <- function(exp) "Unknown class"

#' @rdname obs_rates
#' @export
obs_rates.experiment <- function(exp) {
  as.data.frame(exp[, grep("^r_", names(exp), value = TRUE), drop = FALSE])
}

# parameters -------------------------------------------------------------------
#' Extract parameters values
#'
#' Subsets the columns of an \code{experiment} object that correspond to the
#' values of the parameters.
#'
#' @param exp An object of class \code{experiment}.
#'
#' @return A data frame that is a subset of the inputed \code{experiment} object.
#'
#' @example inst/examples/parameters.R
#' @export
parameters <- function(exp) UseMethod("parameters")

#' @rdname parameters
#' @export
parameters.default <- function(exp) "Unknown class"

#' @rdname parameters
#' @export
parameters.experiment <- function(exp) {
  as.data.frame(exp[, grep("^p_", names(exp), value = TRUE), drop = FALSE])
}

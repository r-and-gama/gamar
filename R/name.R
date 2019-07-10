# name ----------------------------------------------------------------------
#' Get the name of an experiment
#'
#' Retrieves the name of the experiment that an \code{experiment} object is
#' linked to.
#'
#' @param x An object of class \code{experiment}.
#'
#' @return The name of the experiment that the inputed \code{experiment} object
#' is linked to.
#'
#' @example inst/examples/name.R
#' @export
name <- function(x) UseMethod("name")

#' @rdname name
#' @export
name.default <- function(x) "Unknown class"

#' @rdname name
#' @export
name.experiment <- function(x) attributes(x)$experiment

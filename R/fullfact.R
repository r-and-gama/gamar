#' Populate an experiment with values
#'
#' Generates a full experimental design for an experiment.
#'
#' This function is basically a wrapper around the
#' \code{\link[base]{expand.grid}} function applied to all the column of the
#' inputed \code{experiment} object. Any of these columns can be replaced by
#' user-defined vectors of values, see examples.
#'
#' @param xprmt An object of class \code{experiment}.
#' @param ... Vectors used to overwrite columns of \code{xprmt}. These arguments
#'            should be named and their names should match the names of
#'            \code{xprmt}.
#'
#' @return An object of class \code{experiment} with a full factorial design of
#' the inputed \code{experiment}.
#'
#' @example inst/examples/fullfact.R
#' @export
fullfact <- function(xprmt, ...) {
  args <- as.list(match.call(expand.dots = FALSE))
  values <- lapply(args$`...`, eval)
  to_expand <- as.data.frame(xprmt, stringsAsFactors = FALSE)
  if (! is.null(values)) {
    the_names <- names(to_expand)
    to_expand <- c(to_expand[setdiff(the_names, names(values))],
                   values)[the_names]
  }
  new_xprmt <- do.call(expand.grid, lapply(to_expand, unique))
  # sort rows "from left to right"
  new_xprmt[do.call(order, new_xprmt), ]
  # add class and other attributes
  new_xprmt <- rbind(xprmt[1, ], new_xprmt)[-1, ]
  # regenerate row names
  row.names(new_xprmt) <- NULL
  new_xprmt
}

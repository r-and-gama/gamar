#' Tests if an object of type \code{"experiment"} is valid to pass on to gama.
#'
#' @param exp object to be tested
#'
#' @return The function returns `TRUE` or `FALSE` depending on whether its
#' argument is of chatacter type or not
#'
#' @rdname experiment
#' @export
is.experiment <- function(exp) {

  exp1 <- as.data.frame(exp)
  if (any(is.na(exp1[, !names(exp1) %in% "output"])))
    stop("An object `experiment` cannot contain NA value.")
  if (any(is.null(exp)))
    stop("An object `experiment` cannot contain NULL value.")

  attr <- setdiff(c("class", "model", "experiment", "dic_g2r",
                    "dic_r2g"),
                  names(attributes(exp)))
  class <- setdiff(class(exp), c("data.frame", "tbl_df", "tbl", "experiment"))
  length(c(attr, class)) == 0
}

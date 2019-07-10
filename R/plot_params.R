# plot_params --------------------------------------------------------------

#' Visualize values of parameters
#'
#' Plot the parameters that have the greatest variance in the experiments of a
#' model.
#'
#' From the list of all parameters of the experiments that do vary (i.e. not
#' null) the one (up to 3) with the biggest variance are ploted in 3D, 2D and
#' 1D.
#'
#' @param exp An object of class \code{experiment}.
#'
#' @importFrom stats var
#' @importFrom graphics stripchart
#'
#' @return Returns a vector of the variables with highest variances.
#'
#' @example inst/examples/plot_params.R
#' @importFrom plot3D scatter3D scatter2D
#'
#' @export
#'
plot_params <- function(exp) {
  if (nrow(exp) == 0) stop("There is no simulation in this experiment")
  if (nrow(exp) == 1) stop(
    "There is only one simulation in this experiment so no ")

  allvar <- sapply(parameters(exp), var)
  allvar <- sort(allvar[ allvar != 0], decreasing = TRUE)
  if (length(allvar) == 0) return(paste(
    "There is only one set of parameters for these", nrow(exp), "experiments"))
  worthidx <- grep(paste(names(allvar), collapse = "|"), names(exp))
  topidx <- if (length(worthidx) != 0) {
    worthidx[1:min(3, length(worthidx))]
    } else {
      0
    }
  n <- length(topidx)

  # check n the number of parameters to be plotted
  exp <- as.data.frame(exp)
  # if n is equal to 0
  if (n == 0) stop("There is no parameters to plot in this experiment")
  if (n == 1) stripchart(exp[, topidx[1]], xlab = colnames(exp)[topidx[1]])
  if (n == 2) scatter2D(exp[, topidx[1]], exp[, topidx[2]],
                      xlab = colnames(exp)[topidx[1]],
                      ylab = colnames(exp)[topidx[2]])
  if (n == 3) scatter3D(exp[, topidx[1]], exp[, topidx[2]], exp[, topidx[3]],
                        xlab = colnames(exp)[topidx[1]],
                        ylab = colnames(exp)[topidx[2]],
                        zlab = colnames(exp)[topidx[3]],
                        pch = 18, cex = 2,
                        theta = 20, phi = 20
  )
  # returns the parameters of the ploted parameter(s)
  return(topidx)
}

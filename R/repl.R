# repl -------------------------------------------------------------------------
#' Replicate an experiment
#'
#' Produces an \code{experiment} object from the replication of an
#' \code{experiment} object.
#'
#' Parameters \code{times} and \code{exp} are exclusive.
#' If both specified, \code{times} will be taken.
#'
#' @param exp An object of class \code{experiment}.
#' @param times Number of replicates or a named vector of rows to propagate in
#' \code{exp}.
#'
#' @return An object of class \code{experiment} that is the replication of the
#' inputed \code{x} argument.
#'
#' @example inst/examples/repl.R
#' @export
repl <- function(exp, times = NULL) UseMethod("repl")

#' @rdname repl
#' @export
repl.default <- function(exp, times = NULL) "Unknown class"

#' @rdname repl
#' @export
repl.experiment <- function(exp, times = NULL) {
  if (!is.data.frame(exp))
    exp <- as.data.frame(exp)

  if (is.null(times)) {
    return(exp)
  } else if (is.null(names(times))) {
    return (do.call(rbind, lapply(1:times, function(y) exp)))
  } else {
    # check if the row requested is out of bound
    requested_row <- as.numeric(names(times))
    if (sum(requested_row > nrow(exp)) > 0)
      warning(paste0("Row \"", names(times)[requested_row > nrow(exp)],
                     "\" requested is out of bound. Rows of NAs are returned"))
    out <- rbind(exp,
                 do.call(rbind, lapply(1:length(times), function(x){
                   tmp <- exp[names(times)[x], ]
                   tmp <- do.call(rbind, lapply(1:times[x], function(y) tmp))
                   return(tmp)
                  }
                 )))
    return(out)
  }
}

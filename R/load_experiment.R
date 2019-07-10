# get_parameters ---------------------------------------------------------------
get_parameters <- function(x) {
  parameters <- x[["Parameters"]]
  if (is.null(parameters)) return(NULL)
  x2 <- do.call(rbind, parameters)
  x3 <- data.frame(as.list(x2[, "value"]), stringsAsFactors = FALSE)
  x3 <- setNames(x3, x2[, "name"])
  x3
}



# get_variables ----------------------------------------------------------------
get_variables <- function(x) {
  outputs <- x[["Outputs"]]
  if (is.null(outputs)) return(NULL)
  x2 <- do.call(rbind, outputs)
  x3 <- data.frame(as.list(x2[, "framerate"]), stringsAsFactors = FALSE)
  x3 <- setNames(x3, x2[, "name"])
  x3
}



# get_attributes ---------------------------------------------------------------
#' @importFrom stats setNames
get_attributes <- function(x) {
  out <- setNames(do.call(function(...)
    data.frame(..., stringsAsFactors = FALSE),
    as.list(x$.attrs[c("finalStep", "seed", "sourcePath", "experiment")])),
    c("tmax", "seed", "gaml", "experiment"))
  out
}

# load_experiment --------------------------------------------------------------
#' Load an experiment from a GAML file
#'
#' Loads an experiment from a \code{.gaml} file and returns an object of class
#' \code{experiment}.
#'
#' The \code{gamar} package contains an internal collection of GAMA models. These
#' models are specified, at minima, by a \code{.gaml}. Additional files such as
#' shapefile can be used to specify a model, in which case they are in the same
#' directory as the \code{.gaml} file. The internal collection of GAMA models is
#' in the \code{examples} directory of the \code{gamar} package file hierarchy.
#' These models can be accessed with the \code{\link[base]{system.file}}
#' function as explained in the example. \cr
#' Before using the package \code{gamar}, we advice the user to validate their
#' model in Gama Platform.
#'
#' @param exp The name of the experiment to load.
#' @param model The name of the GAML file from which to load the experiment.
#' @example inst/examples/load_experiment.R
#' @importFrom XML xmlToList xmlParse
#'
#' @export
load_experiment <- function(exp, model) {
  test_schar(exp)
  # Reading GAML file:
  message(cat("Loading experiment \"", exp,
                 "\" from file \"", basename(model), "\"...", sep = ""))
  out <- read_gaml_experiment(exp, model)

  # Check if experiment and type requested are valid:
  check_experiment(exp, list("info" = out))

  # Retrieving information:
  make_df_dic <- function(x) {
    if (is.null(x)) return(list(out = data.frame(NULL), dic = NULL))
    the_names <- names(x)
    dic_g2r <- make_dictionary(the_names)
    names(x) <- dic_g2r
    list(out = x, dic_g2r = dic_g2r)
  }
  variables <- make_df_dic(get_variables(out))
  parameters <- make_df_dic(get_parameters(out))
  out_attr <- get_attributes(out)

  # Returning experiment object:
  experiment(parameters$out, variables$out, out_attr$tmax, out_attr$seed,
             exp, out, c(parameters$dic_g2r, variables$dic_g2r))
}

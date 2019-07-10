# constructor ------------------------------------------------------------------
#' @importFrom tools md5sum

new_experiment <- function(parameters, obsrates, tmax, seed,
                           experiment, model, dic_g2r = NULL, output = NA) {

  # Automatically adds "p_" and "r_" prefixes to the parameteres and observation
  # rates. It also does so to the dictionary if provided.

  # Automatically converts periods of obsrates and tmax into integer if there
  # are not.

  # dic_g2r: contains the new names of the parameters and variables and the
  # names of this vector contains the old names. It is the opposite for dic_r2g.

  # exp0 <- gamar:::new_experiment(
  #   data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
  #   data.frame(S = 1, I = 1, R = 1),
  #   1000, 1, "sir", system.file("models", "sir.gaml", package = "gamar"))

  # tmax is set to 1000 by default by headless, seed is set to XXX by headless
  # parameters, obsrates, experiment can be NULL. Only model (gaml file) can't
  # be NULL.

  if (!is.null(parameters)) {
    stopifnot(all(is.data.frame(parameters)))
  }
  if (!is.null(obsrates)) {
    stopifnot(all(is.data.frame(obsrates)))
  }
  if (!is.null(experiment)) {
    stopifnot(all(is.character(experiment),
                  length(experiment) == 1))
  }
  if (!is.null(dic_g2r)) {
    stopifnot(all(is.character(dic_g2r),
                  is.character(names(dic_g2r))))
  }
  if (!is.na(output)) {
    if (nrow(parameters) > 1) {
      stopifnot(all(is.list(output),
                  length(output) > 1,
                  sapply(output, is.data.frame)))
    } else {
      stopifnot(is.data.frame(output))
    }
  }

  stopifnot(all(is.character(model), length(model) == 1) ||
              all(is.list(model), length(model) == 3))

  stopifnot(!is.na(as.numeric(tmax)))
  stopifnot(!is.na(as.numeric(seed)))
  # Generating new names:
  names_param <- names(parameters)
  names_obsrates <- names(obsrates)
  oldnames <- c(names_param, names_obsrates)
  param_newname <- paste0("p_", names_param)
  obsrates_newname <- paste0("r_", names_obsrates)
  newnames <- c(param_newname, obsrates_newname)

  # Dealing with dictionaries:
  if (is.null(dic_g2r)) {
    dic_g2r <- setNames(newnames, oldnames)
    dic_r2g <- setNames(names(dic_g2r), dic_g2r)

  } else {
    stopifnot(all(dic_g2r %in% oldnames))
    sel1 <- which(dic_g2r %in% names_param)
    sel2 <- which(dic_g2r %in% names_obsrates)
    dic_g2r <- c(setNames(paste0("p_", dic_g2r[sel1]), names(dic_g2r[sel1])),
                 setNames(paste0("r_", dic_g2r[sel2]), names(dic_g2r[sel2])))
    dic_r2g <- setNames(names(dic_g2r), dic_g2r)
  }

  # Dealing with obsrates, seed and tmax, converting them into integers if
  # needed:
  if (!all(sapply(obsrates, is.integer))) {
    message(cat(
      "Periods of observation (\"obsrates\") are converted into integers."))
    obsrates[] <- lapply(obsrates, function(x) as.integer(x))
  }

  if (!all(is.integer(tmax))) {
    message(cat(
      "Final time step (\"tmax\") is converted into integer."))
    tmax <- as.integer(tmax)
  }

  # seed has to be numeric
  if (!is.numeric(seed)) {
    message(cat(
      "Seed is converted into numeric."))
    seed <- as.numeric(seed)
  }

  # check if model is a list as a result of read_gaml_experiment already
  if (is.list(model))
    model_info <- list("path" = model$.attrs["sourcePath"],
                       "info" = model,
                       "md5sum" = md5sum(model$.attrs["sourcePath"]))
  else
    model_info <- list("path" = model,
                       "info" = read_gaml_experiment(experiment, model),
                       "md5sum" = md5sum(model))

  # cast parameter types
  if (!is.null(model_info$info$Parameters)){
    types_param <- model_info$info$Parameters[
      lapply(model_info$info$Parameters, "[[", "name") %in%
        c(dic_r2g[param_newname])]
    types <- map_type(unlist(lapply(types_param, function(x) x[["type"]])))

    if (!all(unlist(lapply(parameters, class)) == types)){
      message(cat("Parameters' types are cast according to model definition"))
      functions <- lapply(paste0("as.", types), function(x) match.fun(x))
      mapply(function(n, f){
        parameters[, n] <<- f(parameters[, n])
        invisible()
      },
      seq_along(types), functions)
    }
  }

  # construct output
  if (all(ncol(parameters) == 0, ncol(obsrates) != 0)) {
    out <- setNames(cbind(obsrates,
                          tmax = tmax,
                          seed = seed,
                          output = output),
                    c(grep("r_", newnames, value = TRUE),
                      "tmax", "seed", "output"))
  }
  if (all(ncol(parameters) != 0, ncol(obsrates) == 0)) {
    out <- setNames(cbind(parameters,
                          tmax = tmax,
                          seed = seed,
                          output = output),
                    c(grep("p_", newnames, value = TRUE),
                      "tmax", "seed", "output"))
  }
  if (all(ncol(parameters) == 0, ncol(obsrates) == 0)) {
    out <- data.frame(tmax = tmax,
                          seed = seed,
                          output = output)
  }
  if (all(ncol(parameters) != 0, ncol(obsrates) != 0)) {
    out <- setNames(cbind(parameters,
                          obsrates,
                          tmax = tmax,
                          seed = seed,
                          output = output),
                          c(newnames, "tmax", "seed", "output"))

  }
  out <- structure(out,
                   class      = c("experiment", "tbl_df", "tbl", "data.frame"),
                   model      = model_info,
                   experiment = experiment,
                   dic_g2r    = dic_g2r,
                   dic_r2g    = dic_r2g)
}


# validator --------------------------------------------------------------------
validate_experiment <- function(x) {
  model <- model(x)
  dic_g2r <- attr(x, "dic_g2r")
  dic_r2g <- attr(x, "dic_r2g")
  colnames <- lapply(c(parameters, obs_rates), function(f) names(f(x)))

  check_experiment(name(x), model)

  # check types forced by experiment
  if (!all(!is.null(obs_rates(x)), obs_rates(x) > 0,
           sapply(obs_rates(x), is.integer)))
    stop("The period of observation should be positive integers.")

  if (!all(!is.null(x$tmax),
           x$tmax > 0,
           is.integer(x$tmax)))
    stop("The end steps of simulations should be positive integers.")

  if (!any(is.null(x$seed), is.numeric(x$seed)))
    stop("Seed values should be numeric")

  # check parameter consistency between experiment and gaml

  if (length(setdiff(unlist(colnames), dic_g2r)) > 0)
    stop("Some variables or parameters names are not in the dictionary.")

  if (setequal(dic_g2r, names(dic_r2g)) + setequal(names(dic_g2r), dic_r2g) < 2)
    stop("The dictionaries are inconsistent.")
  if (!is.null(model$info$Parameters)) {
    diff <- setdiff(dic_r2g[colnames[[1]]],
                    sapply(model$info$Parameters,
                           function(x) x[["name"]]))
    if (length(diff) > 1) {
      stop(paste0("The parameters names '", substitute(diff),
                  "' do not correspond to any parameter in the '",
                  basename(model$path), "' file."))
    } else if (length(diff) > 0) {
      stop(paste0("The parameter name '", substitute(diff),
                  "' does not correspond to any parameter in the '",
                  basename(model$path), "' file."))
    }

    # check parameter type consistency between experiment and gaml
    # (selection of the parametes in gaml file by name)

    type_r <- sapply(parameters(x), class)
    names_type_g <- unlist(lapply(model$info$Parameters, "[[", "name"))
    names_type_g <- dic_g2r[names_type_g]
    type_g <- lapply(model$info$Parameters, function(x) x[["type"]])
    type_g <- setNames(type_g, names_type_g)
    type_g <- map_type(unlist(type_g))
    diff <- type_r == type_g[names(type_r)]
    if (!all(diff)) {
      stop(paste0(
        "Data type of parameters don't correspond to those declared in the '",
        basename(model$path), "' file."))
    }
  }

  # check obsrates consistency between experiment and gaml
  if (!is.null(model$info$Outputs)){
    diff <- setdiff(dic_r2g[colnames[[2]]],
                    unlist(lapply(model$info$Outputs, function(x) x[["name"]])))
    if (length(diff) > 1) {
      stop(paste0("The variables names '", substitute(diff),
                  "' do not correspond to any variable in the '",
                  basename(model), "' file."))
    } else if (length(diff) > 0) {
      stop(paste0("The variable name '", substitute(diff),
                  "' does not correspond to any variable in the '",
                  basename(model$path), "' file."))
    }
  }

  # validate snapshot

  current_md5sum <-  md5sum(model(x)$path)

  if (current_md5sum != model$md5sum)
    stop(paste0("Gaml file '", model$path, "' has been changed.
                Please use function 'model<-' to add this gaml file
                to the experiment"))

  return(x)
}



# helper -----------------------------------------------------------------------
#' Constructor of experiments
#'
#' Allows to build an object of class \code{experiment} from individual parts.
#'
#' The class \code{experiment} inherits from the class \code{tibble}
#' (\code{tbl_df}). It contains parameters values as well as periods of
#' observation of the observed variables and it connects to a \code{GAML} file
#' that specifies the full model.
#'
#' TO DO: Explains what \code{dic} is.
#'
#' @param parameters A data frame of numerical values giving the values of each
#'                   parameter (in column), for each simulation (in row).
#' @param obsrates A data frame of positive integer values giving the periods,
#'                 in time steps, at which observed variables are observed.
#'                 Should have the same number of rows as \code{parameters}.
#' @param tmax A positive integer vector, the length of which is either 1 or
#'             equal to the number of \code{parameters} and \code{obsrates}: it
#'             gives the end of simulations, in numbers of time steps.
#' @param seed A numerical vector, the length of which is either 1 or equal to
#'             the number of \code{parameters} and \code{obsrates}: it gives the
#'             seeds of the simulations.
#' @param experiment The name of an experiment of the \code{GAML} file
#'                   \code{model}.
#' @param model The path to a \code{GAML} file.
#' @param dic A named vector of character strings. The values and the names of
#'            this vector should be consistent with the names of
#'            \code{parameters}, \code{obsrates} as well as the variables and
#'            parameters defined in the \code{model} \code{GAML} file. See
#'            Details for more information.
#'
#' @return An object of class \code{experiment}.
#'
#' @export
#'
#' @example inst/examples/experiment.R
#'
experiment <- function(parameters, obsrates, tmax, seed, experiment, model,
                       dic = NULL) {
  validate_experiment(new_experiment(parameters, obsrates, tmax, seed,
                                     experiment, model, dic))
}

# generate xml tags for each parameter
generate_param <- function(param, names, types) {
  mapply(function(p, n, t){
    c(name = n,
      type = t,
      value = p)
  },
  unlist(param), names, types,
  SIMPLIFY = FALSE)
}

# ------------------------------------------------------------------------------
# generate xml tags for each observation rate
generate_obsrate <- function(obsrate, names) {
  mapply(function(p, n, i){
                c(id = i - 1,
                  name = n,
                  framerate = p)
              },
         unlist(obsrate), names, seq_along(unlist(obsrate)),
         SIMPLIFY = FALSE)

}

# save_to_gama -----------------------------------------------------------------
#' Save an experiment plan to a GAMA XML file
#'
#' Save an object of class \code{experiment} to an XML file GAMA-compliant.
#'
#' @param exp An object of class \code{experiment}.
#' @param filename name of XML parameter file. If not
#'                 specified, name of `exp`` is used.
#' @param path Path to save `filename`. If not specified, current working
#'                directory is used.
#'
#'
#' @importFrom XML xmlToList xmlParse xmlOutputDOM saveXML
#' @example inst/examples/save_to_gama.R
#' @rdname save_to_gama
#' @export

save_to_gama <- function(exp, filename = NULL, path = NULL)
  UseMethod("save_to_gama")

#' @rdname save_to_gama
#' @export
save_to_gama.default <- function(exp, filename, path)
                        "Unknown class"

#' @rdname save_to_gama
#' @export
save_to_gama.experiment <- function(exp, filename = NULL, path = NULL) {

  exp <- validate_experiment(exp)
  params <- lapply(seq_len(nrow(parameters(exp))),
                   function(x) as.character(parameters(exp)[x, ],
                                            stringsAsFactors = FALSE))
  param_names <- attr(exp, "dic_r2g")[names(parameters(exp))]

  obsrates <- lapply(seq_len(nrow(obs_rates(exp))),
                     function(x) as.character(obs_rates(exp)[x, ],
                                              stringsAsFactors = FALSE))
  obsrates_names <- attr(exp, "dic_r2g")[names(obs_rates(exp))]

  simulations <- as.list(as.data.frame(rbind(id = row.names(exp),
                       seed = exp$seed,
                       finalStep = exp$tmax,
                       sourcePath = model(exp)$path,
                       experiment = name(exp)),
                       stringsAsFactors = FALSE))
  names(simulations) <- row.names(exp)

  types <- list(unlist(lapply(model(exp)$info$Parameters, "[[", "type")))

  xmlFile <- xmlOutputDOM(tag = "Experiment_plan")

  mapply(function(simul, param, obsrate, type) {

    names(simul) <- c("id", "seed", "finalStep", "sourcePath", "experiment")
    xmlFile$addTag("Simulation", attrs = simul, close = FALSE)

    param_lst <- generate_param(param, param_names, type)
    xmlFile$addTag("Parameters", close = FALSE)
    lapply(param_lst, function(x) xmlFile$addTag("Parameter", attrs = x))
    xmlFile$closeTag()

    obsrate_lst <- generate_obsrate(obsrate, obsrates_names)
    xmlFile$addTag("Outputs", close = FALSE)
    lapply(obsrate_lst, function(x) xmlFile$addTag("Output", attrs = x))
    xmlFile$closeTag()

    xmlFile$closeTag()
  },
  simulations, params, obsrates, types)

  if (is.null(filename)) filename <-  paste0(name(exp), ".xml")
  if (is.null(path)) path <- getwd()
  parameter_xml_file <- paste0(path, "/", filename)
  saveXML(xmlFile$value(), file = parameter_xml_file)
  normalizePath(parameter_xml_file)
}

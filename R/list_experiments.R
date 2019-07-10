#' List the experiments of a model and their types
#'
#' List the experiments of a given model.
#'
#' @param file Path to a \code{.gaml} file.
#'
#' @example inst/examples/list_experiments.R
#' @export
list_experiments <- function(file){
  if (!file.exists(file)) {
    stop(paste0("File \"", file, "\" does not exist."))
  }

  gaml <- paste(readLines(file, warn = FALSE), collapse = "\n")
  exp_info <- gregexpr("\\nexperiment (.*?)\\{", gaml)
  exp_info <- exp_info[[1]]
  if (any(exp_info == -1)){
    return(NULL)
  }
  exps <- lapply(seq_along(exp_info), function(x) {
    exp <- substr(gaml, exp_info[x],
                  exp_info[x] + attr(exp_info, "match.length")[x])
    exp <- trimws(gsub("\\nexperiment|\\{|\\n+$", "", exp))
    exp <- gsub("\"", "", exp)
    if (grepl("type:", exp)) {
      experiment <- trimws(substr(exp, 1, regexpr("type:", exp) - 1))
      type <- trimws(substr(exp, regexpr("type:", exp) + 5, nchar(exp)))
      if (grepl("\\s+", type))
        type <- trimws(substr(type, 1, regexpr("\\s+", type)))
      cbind(experiment, type)
    } else {
      cbind("experiment" = exp, "type" = "gui")
    }
  })
  exp_info <- as.data.frame(do.call(rbind, exps), stringsAsFactors = FALSE)
  # test if there is special character in experiment name
  test_schar(exp_info$experiment)
  exp_info
}

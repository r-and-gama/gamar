# map gama and R data types
map_type <- function(x) {
  types <- c("INT" = "integer",
             "FLOAT" = "numeric",
             "STRING" = "character",
             "BOOLEAN" = "logical",
             "UNDEFINED" = "character")
  unlist(lapply(x, function(y) types[[y]]))
}

# read gaml experiment ---------------------------------------------------------
read_gaml_experiment <- function(exp, model) {
  tmp <- tempfile(fileext = ".xml")
  logFile <- paste0(getwd(), "/read_gaml.log")

  exp <- paste0("\'", exp, "\'", collapse = "")
  model <- paste0("\'", model, "\'", collapse = "")
  stderrFile <- tempfile(fileext = ".stderr")
  stdoutFile <- tempfile(fileext = ".stdout")
  run <- list()
  run$exitStatus <- system2(command = 'java',
          args = c('-jar',
                   getOption("gamar.startjar"),
                   '-Xms',
                   getOption("gamar.Xms"),
                   '-Xmx',
                   getOption("gamar.Xmx"),
                   '-Djava.awt.headless=true org.eclipse.core.launcher.Main',
                   '-application msi.gama.headless.id4 -v -hpc 2',
                   '-xml',
                   exp,
                   model,
                   tmp,
                   '>',
                   shQuote(stdoutFile),
                   '2>',
                   shQuote(stderrFile)))

  if (file.exists(getOption("gamar.log")))
    file.copy(from = getOption("gamar.log"),
              to = logFile)
  run$stdout <-  readLines(stdoutFile)
  run$stderr <-  readLines(stderrFile)

  if (length(run$stdout) > 0 & run$exitStatus > 0)
    message(paste0("An error has occurred in gama.\nSee the log file", logFile))

  unlink(getOption("gamar.workspace"), TRUE, TRUE)
  unlink(c(stdoutFile, stderrFile))

  if (file.exists(tmp)){
    xml <- XML::xmlToList(XML::xmlParse(tmp))$Simulation
    unlink(tmp)
    return(xml)
  }
  stop(paste0("Gama fails to read your experiment"))
}

# test special characters ------------------------------------------------------
test_schar <- function(x) {
  if (any(grepl("[\\']", x))) {
    stop(paste0("The gamar package does not support the specials characters `<`",
                ", `>`, `&` and `'` in parameters,",
                " outputs and experiments names."))
  }
}

# Check if a requested experiment is valid -------------------------------------
# For a requested experiment to be valid, we need
# * the name to exist in the file;
# * the experiment to be of type "GUI".
check_experiment <- function(exp, model) {
  model_info <- model$info
    # check if the requested experiment is present in the file:
  if (!exp %in% model_info$.attrs["experiment"])
    stop(paste0("There is no experiment named \"", exp, "\" in ",
                basename(model_info$.attrs["sourcePath"])))
  test_schar(exp)
  invisible(0)
}

# make_dictionary --------------------------------------------------------------
#' @importFrom stats setNames
make_dictionary <- function(x) {
  dic <- gsub("[[:space:]]|[[:punct:]]", "_", x)
  dic <- gsub("_+", "_", dic)
  setNames(dic, x)
}

# Defines the GAMA repository --------------------------------------------------
gama_repo <- function(repo = NULL) {
  if (! is.null(repo)) options(gamar.repo = repo)
}

# Test if operating system is Windows ------------------------------------------
isWindows <- function() {
  return (Sys.info()["sysname"] == "Windows")
}

# Returns the OS ---------------------------------------------------------------
get_os <- function(){
  os <- paste0(Sys.info()["sysname"])
  if (is.null(os)){
    if (grepl("^darwin", R.version$os))
      os <- ""
    if (grepl("linux-gnu", R.version$os))
      os <- "Linux"
  }
  os
}

# Gives distrib as a function of the OS ----------------------------------------
#' In function of remote distribution of the OS returns the path
#' @noRd
gama_remote_distrib <- function() {
  switch(get_os(),
         "Darwin"  = paste0(options("gamar.repo"),
                            options("gamar.default.gama.osx")),
         "Windows" = paste0(options("gamar.repo"),
                            options("gamar.default.gama.win64")),
         "Linux"   =  paste0(options("gamar.repo"),
                             options("gamar.default.gama.linux")))
}

# Downloads gama ---------------------------------------------------------------
#' @importFrom utils download.file unzip untar
#' @importFrom downloader download
download_gama <- function() {
  distrib <- gama_remote_distrib()
  expDir  <- gama_local_distrib_path()
  path <- paste0(options("gamar.temp_dir"), "/")
  #path_dist <- paste0(path, "out/", basename(expDir))
  path_test <- dirname(expDir)
  distrib_file <- paste0(path, "downloaded_gama.tgz")

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  path_test <- switch(get_os(),
          "Darwin" = paste0(path_test, "/",
                            options("gamar.default.gama.osx.zip.appdir")),
          "Windows" = paste0(path_test, "/",
                             options("gamar.default.gama.osx.zip.appdir")),
          "linux" = paste0(path_test, "/",
                           options("gamar.default.gama.osx.zip.appdir")))
  download(distrib, distrib_file,  mode = "wb")
  untar(distrib_file, exdir = path_test)
  gama_app <- switch(get_os(),
                     "Darwin" = options("gamar.default.gama.osx.appdir"),
                     "Windows" = options("gamar.default.gama.win.appdir"),
                     "linux" = options("gamar.default.gama.linux.appdir"))
  expDir
}

# Setup GAMA UI ----------------------------------------------------------------
setup_gama_ui <- function() {
  defaultjar <- ""
  repeat{
    message(cat("Give the path of Gama platform or [Q]uit:"))
    answer <- readline()
    if (answer[1] == "Q" | answer[1] == "q" ) return(NA)
    defaultjar <- is_gama_installed(answer)
    if (defaultjar) break
    else {
      warning("Gama is not found at the specified location")
      warning("Please give the correct location")
    }
  }
  answer
}

# download GAMA when necessary -------------------------------------------------
#' Download GAMA and configure
#'
#' Prompt the user to download GAMA if he wish to and then configures
#' \code{gamar}, linking it to a GAMA engine installed on the system.
#'
#' @param path path to the applicatation Gama Platform
#'
#' @examples
#' \dontrun{
#' # For interactive usage:
#' setup_gama()
#'
#' # If input a path, the function will not be interactive:
#' setup_gama(getwd())
#' }
#'
#' @export
setup_gama <- function(path = NA) {
  if (!is.na(path)) {
    defpath(path)
    return(NA)
  }
  if (is_gama_installed()) {
    message(cat("Gama is already installed, do you want to setup a new one? "))
    answer <- toupper(readline("[Y]es/[N]"))
    if (answer[1] == "N") return(NA)
  }

  repeat {
    message(cat("Do you want to download the last version of Gama? "))
    answer <- toupper(readline(" [Y]es/[N] or [K]eep my current version."))
    if (answer[1] == "Y" | answer[1] == "N" | answer[1] == "K") break
    else print("Sorry, I din't understand... try again")
  }
  if (answer[1] == "Y") {
    gama_path <- download_gama()
    defpath(gama_path)
  }
    if (answer[1] == "K") {
       gama_path <- setup_gama_ui()
       if (is.na(gama_path)) return(NA)
       defpath(gama_path)
  }
}

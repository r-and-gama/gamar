# Environment Variables

set_environment_variables <- function() {
  gamar_workspace <- paste0(getwd(), "/workspace")
  options(gamar.workspace                     = gamar_workspace,
          gamar.temp_dir                      = paste0(gamar_workspace, "/temp"),
          gamar.gama_dir                      = paste0(gamar_workspace, "/gama"),
          gamar.default.gama.win64            = "/gama_1_8_final_win64.tgz",
          gamar.default.gama.win32            = "/gama_1_8_final_win64.tgz",
          gamar.default.gama.win.appdir       = "C:/Program Files/gama",
          gamar.default.gama.win.zip.appdir   = "gama",
          gamar.default.gama.osx              = "/gama_1_8_final_osx.tgz",
          gamar.default.gama.osx.appdir       = "/Applications/Gama.app",
          gamar.default.gama.osx.zip.appdir   = "",
          gamar.default.gama.linux            =
            "/gama_1_8_final_linux64.tgz",
          gamar.default.gama.linux.appdir     = "/usr/local/gama",
          gamar.default.gama.linux.zip.appdir = "gama",
          gamar.repo                          = "http://51.255.46.42/releases",
          gamar.gama.path                     = "UNKNOWN",
          gamar.startjar                      = "UNKNOWN",
          gamar.plugins                       = "UNKNOWN",
          gamar.log                           = paste0(getwd(),
                                                       "/workspace/.metadata/.log"))
}

# ------------------------------------------------------------------------------
#' In function of remote distribution of the OS returns the path
#' @noRd
gama_local_distrib_path <- function() {
  path <- switch(get_os(),
         "Darwin"  = options("gamar.default.gama.osx.appdir"),
         "Windows" = options("gamar.default.gama.win.appdir"),
         # to complete C:\Program Files\
         "Linux"  = options("gamar.default.gama.linux.appdir"))
  # to complete
  unlist(path)
}

# -------------------------------------------

init_gama_path <- function(path) {
  os <- paste0(get_os())
  subpath <- ifelse(os == "Darwin", "/Contents/Eclipse", "")
  res <- paste0(path, subpath, "/plugins")
  ifelse(dir.exists(res), res, NA)
}

# -------------------------------------------

init_gama_jar <- function(path) {
  gamapath <- init_gama_path(path)
  if (is.na(gamapath))
    return(NA)
  plugins <- grep("org.eclipse.equinox.launcher_.*", dir(gamapath),
                  value = TRUE)
  res <- paste0(gamapath, "/", plugins)
  ifelse(file.exists(res), res, NA)
}

# Test version -----------------------------------------------------------------
#' Extract Gama version
#' @noRd
gama_version <- function(path) {
  v_info <- readLines(
    paste0(path, "/Contents/Eclipse/Configuration/config.ini"))
  v_info <- v_info[10]
  v_info <- gsub("[[:alpha:]]|=", "", v_info)
  v_info <- as.numeric(v_info)
}

# Configure gama ---------------------------------------------------------------
#' Configure GAMA path and Java heap size
#'
#' @param path Path to Gama
#' @param Xmx Maximum heap size
#' @param Xms Initial heap size
#' @examples
#' \dontrun{
#' defpath(path = "/Applications/Gama.app/", Xmx = "4096m", Xms = "512m")
#' }
#' @export
defpath <- function(path, Xmx = "2048m", Xms = "512m") {
  defaultjar <- init_gama_jar(path)
  version <- gama_version(path)
  if (is.na(defaultjar)) {
    stop("Gama configuration failed!")
  }
  else {
    options(gamar.startjar = defaultjar,
            gamar.Xmx = Xmx,
            gamar.Xms = Xms,
            gamar.gama.path = path)
    message("Gama configuration succeed!")
      if (version < 1.8) {
        stop(
          "Gama version should be 1.8 or superior. Please use `setup_gama()` ",
          "to download or setup a new version of GAMA or see ",
          "https://gama-platform.github.io/ to download GAMA")
      }
  }
}


# Interface to download GAMA if necessary --------------------------------------
#' Test if GAMA is intalled
#'
#' Test if GAMA is intalled and is correctly linked to the application Gama
#' platform
#'
#' @param path path to the application Gama platform
#'
#' @examples
#' \dontrun{
#' # For examples, for MacOs, it can be:
#' is_gama_installed(path = "/Applications/Gama.app")
#' }
#' @export
is_gama_installed <- function(path = unlist(options("gamar.gama.path"))) {
  options("gamar.startjar") != "UNKNOWN" | (dir.exists(path) &
                                             (!is.na(init_gama_jar(path))))
}

# On attach --------------------------------------------------------------------

.onAttach <- function(...) {
  set_environment_variables()
  packageStartupMessage("Welcome to gamar v0.0.1!\n")
  packageStartupMessage("GAMA platform needs to be installed on your machine.")
  packageStartupMessage(
    "See http://www.gama-platform.org for more instructions about GAMA.\n")
  pehaps_path <- gama_local_distrib_path()
    if (is_gama_installed(pehaps_path)) {
      version <- gama_version(pehaps_path)
      packageStartupMessage(
        paste0("-- note that GAMA platform was found at ", pehaps_path, "\n"))
      if (version < 1.8) {
        packageStartupMessage(
          "Gama version should be 1.8 or superior. Please use `setup_gama()` ",
          "to download or setup a new version of GAMA or see ",
          "https://gama-platform.github.io/ to download GAMA")
      } else {
        defpath(pehaps_path)
      }
    } else {
      packageStartupMessage(
        "WARNING: GAMA platform not found! Proceed to the setup")
      packageStartupMessage(
        "-------: Use the command `setup_gama()` to setup or download GAMA")
  }
}

# On detach --------------------------------------------------------------------

.onDetach <- function(...) {
  options(gamar.workspace                 = NULL,
          gamar.temp_dir                  = NULL,
          gamar.gama_dir                  = NULL,
          gamar.default.gama.win          = NULL,
          gamar.default.gama.win.appdir   = NULL,
          gamar.default.gama.osx          = NULL,
          gamar.default.gama.osx.appdir   = NULL,
          gamar.default.gama.linux        = NULL,
          gamar.default.gama.linux.appdir = NULL,
          gamar.repo                      = NULL,
          gamar.plugins                   = NULL,
          gamar.startjar                  = NULL,
          gamar.Xmx                       = NULL,
          gamar.Xms                       = NULL,
          gamar.gama.path                 = NULL)
}

#' Clean output of run_gama
#'
#' Remove folder `snapchot`` and the file `console-outputs` if empty.
#'
#' @param output_dir path to the output of gama.
#' @noRd
clean_output <- function(output_dir) {

  # remove empty folder snapshot
  snap_path <- paste0(output_dir, "/snapshot")
  if (file.exists(snap_path) & length(dir(snap_path)) == 0) {
    file.remove(snap_path)
  }

  # remove empty file console_outputs
  path_file <- grep("console-outputs", dir(output_dir), value = TRUE)
  path_file <- paste0(output_dir, "/", path_file)
  out_file <- file.info(path_file)
  empty_file <- out_file[which(out_file$size == 0), ]
  empty_file <- rownames(empty_file)
  file.remove(empty_file)
}

################################################################################
#' Run GAMA on a XML file
#'
#' From a XML file containing an experiment plan, send and run it in GAMA and
#' returns one XML file by simulation containing the output.
#'
#' @param parameter_xml_file path to an XML file containing an experiment.
#' @param hpc numeric, number of cores
#' @param output_dir path to saved the output of gama. If not specified, current
#'                   working directory will be used. If `output_dir` doesn't
#'                   exist, it will be created.
#'
#' @example inst/examples/call_gama.R
#' @export
call_gama <- function(parameter_xml_file, hpc, output_dir = "") {
  if (output_dir == "")
    output_dir <- getwd()

  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)
  logFile <- paste0(output_dir, "/run_gama.log")

  cat("Running experiment plan... \n")
  parameter_xml_file <- paste0("\'", parameter_xml_file, "\'", collapse = "")
  output_dir_gama <-  paste0("\'", output_dir, "\'", collapse = "")
  stderrFile <- tempfile(fileext = ".stderr")
  stdoutFile <- tempfile(fileext = ".stdout")
  run <- list()
  run$exitStatus <- system2(
                        command = 'java',
                        args = c('-jar',
                                 getOption("gamar.startjar"),
                                 '-Xms',
                                 getOption("gamar.Xms"),
                                 '-Xmx',
                                 getOption("gamar.Xmx"),
                                 '-Djava.awt.headless=true org.eclipse.core.launcher.Main',
                                 '-application msi.gama.headless.id4',
                                 '-hpc',
                                 hpc,
                                 '-v',
                                 parameter_xml_file,
                                 output_dir_gama,
                                 '>',
                                 shQuote(stdoutFile),
                                 '2>',
                                 shQuote(stderrFile)))

  run$stdout <-  readLines(stdoutFile)
  run$stderr <-  readLines(stderrFile)
  if (file.exists(getOption("gamar.log")))
    file.copy(from = getOption("gamar.log"), to = logFile)

  unlink(c(stdoutFile, stderrFile))
  unlink(getOption("gamar.workspace"), TRUE, TRUE)

  if (length(run$stdout) > 0 & run$exitStatus > 0)
    message(paste0("An error has occurred in gama.\nSee the log file", logFile))

  if (run$exitStatus > 0)
      stop(paste0("Gama fails to run your experiment."))

  # remove empty output
  clean_output(output_dir)

  return(normalizePath(dir(path = output_dir,
             pattern = "simulation-outputs[[:digit:]]+\\.xml",
             full.names = TRUE)))
}

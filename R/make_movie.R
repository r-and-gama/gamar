# Make movie -------------------------------------------------------------------
#' Create GIF animations
#'
#' From the output of the function \code{run_experiment} with display, create
#' a .gif file of the output for one specific columns. One gif will be created
#' for each experiment (rows in the input object).
#'
#' The function uses the package \code{animation} to generate the .gif file(s).
#' Please take a look at the documentation of the package and to the website
#' \url{ https://yihui.name/animation} for more information on the usage and
#' the installation of this package.\cr\cr
#' The arguments \code{autoplay}, \code{interval}, \code{loop},
#' \code{autobrowse} and \code{...} are used to control the behaviour of the
#' animation, see \code{?animation::ani.options} for more details..
#'
#' @param exp an object of class experiment.
#' @param name_col character string, name of the column containing the display
#'   information (format .png).
#' @param output_path path to store the .gif file generated, if \code{NULL} will
#'  take the path store in the attributes of each data frame contained in the
#'  column \code{output}.
#' @param autoplay logical, whether to autoplay the animation when the HTML
#' page is loaded (default to be TRUE).
#' @param interval  a positive number to set the time interval of the animation
#' (unit in seconds); default to be 0.01.
#' @param loop logical or numeric, Number of times the GIF animation is to
#' cycle; default TRUE (infinite loop)
#' @param autobrowse logical: whether auto-browse the animation page immediately
#' after it is created?; default to be FALSE
#' @param ... arguments in tag = value form, or a list of tagged values.
#'  The tags usually come from the animation parameters.
#'
#' @return path the gif file(s) invisibly.
#'
#' @importFrom animation ani.options im.convert
#' @importFrom stats na.omit
#'
#' @example inst/examples/prey_predator.R
#' @export
make_movie <- function(exp, name_col, output_path = NULL, autoplay = TRUE,
                       interval = .01, loop = TRUE, autobrowse = FALSE, ...){
  m <- lapply(seq_len(nrow(exp)), function(i) {
  # creates path
   out_path <- dirname(attr(exp$output[[i]], "path"))
   snap_dir <- paste0(out_path, "/snapshot/")
   if (is.null(output_path)) output_path <- out_path
   movie_dir <- paste0(output_path, "/movie/")
   if (!(dir.exists(movie_dir))) dir.create(movie_dir)
  # make .gif files for each row (for one specific columns)
   gif_files <- exp$output[[i]][, name_col, drop = TRUE]
   gif_files <- sapply(seq_along(gif_files), function(x)
     ifelse(file.exists(gif_files[x]), gif_files[x],
            ifelse(is.na(gif_files[x]), NA, paste0(snap_dir, gif_files[x]))))
   output <-
     paste0(movie_dir, "animation_", attr(exp, "dic_r2g")[name_col], i,".gif")
   animation::ani.options(interval = interval, loop = loop,
                          autobrowse = autobrowse, autoplay = autoplay,
                          nmax = length(gif_files), ...)
   suppressMessages(
     a <- animation::im.convert(files = na.omit(gif_files), output = output))
   output
  })
  invisible(unlist(m))
}

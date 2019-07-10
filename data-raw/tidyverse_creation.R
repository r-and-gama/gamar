library(dplyr)
# ------------------------------------------------------------------------------
body_function <- function(fct, args1) {

  body_arg <- paste0("(", paste(args1, collapse = ", "), ", ...)")

  lgth <- length(args1)
  if (lgth > 0) {
    body_f <- paste0(
      '\told_attr <- purrr::keep(attributes(', args1[1],
      '), names(attributes(', args1[1], ')) %in% ',
      'c("dic_r2g", "dic_g2r", "experiment", "model", "class")) \n ',
      '\t', args1[1], ' <- as.data.frame(', args1[1], ') \n')
    if (lgth > 1) {
      f <- function(x) {
        paste0(
          '\told_attr <- c(old_attr, purrr::keep(attributes(', x,
          '), names(attributes(', x, ')) %in% ',
          'c("dic_r2g", "dic_g2r", "experiment", "model", "class"))) \n ',
          '\told_attr <- purrr::keep(old_attr, duplicated(old_attr) == FALSE)\n',
          '\t', x, ' <- as.data.frame(', x, ') \n')
      }
      body_f <- paste0(c(body_f, lapply(args1[-1], f)), collapse = "")
    }
  }

  body_f <- paste0(body_f, '\t.data <- dplyr::', fct, body_arg, "\n \t")
  body_f <- paste0(
      body_f,
      'attributes(.data) <- append( purrr::discard(attributes(.data), ',
      'names(attributes(.data)) == "class"), old_attr) \n \t',".data")
}

# ------------------------------------------------------------------------------
# arg1 : argument(s) of class experiment
doc_function <- function(args1) {

  fct_doc <- paste0(
    "#' Tidyverse methods for experiment objects (remove .experiment suffix!) \n",
    "#' \n",
    "#' Tidyverse methods for experiment objects. Use these methods without the ",
    ".experiment suffix and after loading the tidyverse package with the generic ",
    "(or after loading package tidyverse). \n#' \n")
  if (length(args1) > 0) {
    f <- function(x) {
      paste0("#' @param ", x, " data object of class \\link{experiment} \n")}
    fct_doc <- paste0(c(fct_doc, sapply(args1, f)), collapse = "")
    }
  paste0(c(fct_doc, "#' @param ... other arguments \n",
           "#' @name tidyverse \n"), collapse = "")
}

# ------------------------------------------------------------------------------
tidy_fct <- function(name) {
  # function argument
  argument_name <- names(formals(eval(sym(name))))
  args1 <- grep("\\.data|\\.tbl|^x$|^y$|^data$|^tbl$", argument_name, value = TRUE)
  args2 <- grep(".data|.tbl|x|y|data|tbl", argument_name, value = TRUE,
                invert = TRUE)

  # function
  fct_name <- paste0(name, ".experiment <- ",
                     paste0("function(", paste(args1, collapse = ", "),
                            ", ...) "))
  body_f <- body_function(name, args1)

  # function documentation
  doc_f <- doc_function(args1)

  # total
  function_tot <- paste0(doc_f, fct_name, "{ \n", body_f, "\n }")
}

# ------------------------------------------------------------------------------
add_register_method <- function(lst_name) {
  lst <- lapply(lst_name, function(x) {
    paste0('\t register_s3_method("dplyr", "', x, '", "experiment")')
  })
  tot <- paste0("register_all_s3_methods <-  function() {\n",
                paste(lst, collapse = "\n"), "\n}\n")
}

# ------------------------------------------------------------------------------
# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
# Thu Apr 19 10:53:24 CEST 2018 (adapted)
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
}

# On load ----------------------------------------------------------------------
.onLoad <- function(libname, pkgname) {
  register_all_s3_methods()
}

# Tests ------------------------------------------------------------------------

make_test <- function(name) {
  argument_name <- names(formals(eval(sym(name))))
  args1 <- grep("\\.data|\\.tbl|^x$|^y$|^data$|^tbl$", argument_name,
                value = TRUE)

  if (length(args1) == 1) {
    test <- paste0(name, "(exp)")
  } else {
    test <- paste0(name, "(exp, exp)")
  }
  paste0('\ttestthat::expect_s3_class(', test, ', "experiment")')
}

# ------------------------------------------------------------------------------
# The function `ls(getNamespaceInfo("dplyr", "exports"))` returns all the
# function exported by the package `dplyr`. In this list I only select the
# function that concern data frame and that the user may use to adapt for an
# object of class 'experiment'.
# I did not choose the functions having a direct impact on the name of the column
# like the function 'rename' for example.
# I also choose the function based on the list of function that the package `sf`
# use.
fct <- ls(getNamespaceInfo("dplyr", "exports")) %>%
  grep(
    "sample|distinct|summarise|slice|transmute|group|arrange|filter|select|mutate|join",
    ., value = TRUE) %>%
  grep("sql|select_var|n_distinct|group_cols", ., invert = TRUE, value = TRUE)

# Make tidyverse.R
dplyr_fct <- c(
  lapply(fct, function(x) tidy_fct(x)),
  add_register_method(fct),
  paste0("# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R \n",
  "# Thu Apr 19 10:53:24 CEST 2018 (adapted) \n", "register_s3_method <- ",
         capture.output(eval(sym("register_s3_method"))) %>%
           paste(collapse = "\n"), "\n\n",
         ".onLoad <- ",
         capture.output(eval(sym(".onLoad"))) %>%
           paste(collapse = "\n"))
)
writeLines(capture.output(cat(paste(dplyr_fct, collapse = "\n\n"))),
           con = paste0(getwd(), "/R/tidyverse.R"))

# Make tests/testthat/test_tydiverse.R
dplyr_test <- c(
  paste0('library(dplyr)\n\ntest_that("Tests tidyverse", {',
         '\n\tdf <- data.frame("S0" = rep(999, 5), "I0" = rep(1, 5), "R0" = rep(0, 5),',
         '"beta" = rep(1.5, 5), "gamma" = runif (5, 0, 1), "S" = rep(1, 5), ',
         '"I" = rep(1, 5), "R" = c(1: 5), "a" = rep(1000, 5), "b" = rep(1, 5)) \n',
         '\texp <- as_experiment(df, parameters = c("S0", "I0", "R0", "beta", "gamma"),',
         'obsrates = c("S", "I", "R"), tmax = "a", seed = "b", experiment = "sir", ',
         ' model = system.file("models", "sir.gaml", package = "gamar"))'),
  lapply(fct %>% grep("_", ., value = TRUE, invert = TRUE), function(x) make_test(x)),
  "})"
)

writeLines(capture.output(cat(paste(dplyr_test, collapse = "\n"))),
           con = paste0(getwd(), "/tests/testthat/test_tidyverse.R"))

rm(list = ls())

.buildsite <- function() {
  pkgdown::build_site()

# loading index.html:
  index_file <- "docs/index.html"
  index <- readLines(index_file)

# removing the link to bioconductor:
  sel <- grep("bioconductor", index)
  sel <- sel:(sel + 1)
  index <- index[-sel]

# rewriting index.html:
  writeLines(index, index_file)

# removing output folders:
  unlink("docs/reference/sir", TRUE)
  unlink("docs/reference/sir_*", TRUE)
  unlink("docs/reference/testsir*", TRUE)
  unlink("docs/reference/my_sir_model*", TRUE)
}

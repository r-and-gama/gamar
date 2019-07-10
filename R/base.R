# indexes_first_and_last -------------------------------------------------------
#' @param x A vector of characters.
#' @param n The number of elements to extract from the vector x. Should be > 1.
#'
#' @return A subvector of n elements of x
#'
#' @details If n = 2, it returns the first and the last elements, if n = 3, it
#'          returns the first 2 and the last elements, if n = 4, it returns the
#'          first 2 and the last 2 elements, if n = 5, it returns the first 3
#'          and the last 2 elements, and so on...
#'
#' @noRd
indexes_first_and_last <- function(x, n) {
  l <- length(x)
  x[c(1:rep(1:l, each = 2)[n], rep(l:1, each = 2)[n - 1]:l)]
}

# get_width --------------------------------------------------------------------
#' @param x A vector of characters.
#' @param n The targeted width, in number of characters, we would like.
#'
#' @return The actual width, in number of characters, we get.
#'
#' @noRd
get_width <- function(x, n) {
  x <- nchar(indexes_first_and_last(x, n))
  sum(x) + length(x) - 1
}

# names_of_left_and_right ------------------------------------------------------

#' @param x A vector of characters.
#' @param th The targeted width, in number of characters, we would like.
#'
#' @return A list of two vectors of characters. The first element corresponds to
#'         the left part and the second element corresponds to the right part.
#'
#' @noRd
names_of_left_and_right <- function(x, th) {
  tmp <- sapply(2:length(x), get_width, x = x) > th
  if (all(tmp)) tmp <- x[c(1, length(x))]
  else {
    if (all(! tmp)) tmp <- x
    else tmp <- indexes_first_and_last(x, which(tmp)[1])
  }
  sel <- 1:round(length(tmp) / 2)
  list(tmp[sel], tmp[-sel])
}

# insert_middle ----------------------------------------------------------------

#' @param x A data frame.
#' @param n The width, in number of characters we wish the data frame.
#'
#' @return A data frame with reduced number of columns.
#'
#' @examples
#' insert_middle(as.data.frame(exp5), 20)
#'
#' @noRd
insert_middle <- function(x, n, digits = 4) {
  a <- names_of_left_and_right(names(x), n)
  if (sum(sapply(a, length)) < length(x)) {
    left <- x[, a[[1]], drop = FALSE]
    right <- x[, a[[2]], drop = FALSE]
    middle <- setNames(data.frame(".", ".", ".",
                                  stringsAsFactors = FALSE), rep(".", 3))
    res <- cbind(left, middle, right)
    res_long <- nchar(names(res)) > 15
    names(res)[res_long] <- paste0(substr(names(res)[res_long], 1, 15),
                                   "\u2026")
    return(res)
  }
  x
}

# print_output -----------------------------------------------------------------
#' @param x A data frame with a column "output"
#' @noRd
print_output <- function(x) {
  col_ <- x$output
  if (length(col_) > 0) {
    n_col <-  sapply(seq_along(col_), function(j) {
                       ifelse(is.na(col_[j]), NA,
                              paste0("<", class(col_[[j]]), "[",
                                     paste(dim(col_[[j]]), collapse = ","), "]>"
                                     ))
                       })
    x[, "output"] <- n_col
    x
  }
  x
}

# print.experiment method ------------------------------------------------------
#' @importFrom utils head tail
#' @export
print.experiment <- function(x, interspace = 3, n = 6, digits = 4,
                             nchar = 50, ...) {

  attrs <- attributes(x)

  print_info <- function() {
    cat(  "experiment name: ", attrs$experiment,
          "\ninput gaml file: ", attrs$model$path, "\n")
  }

  if (ncol(x) < 1) {

    cat(
      "Experiment without any simulation,",
      "tunable parameter or observed variable\n")
    print_info()

  } else {

    s <- function(x) ifelse(x > 1, "s", "")
    param <- parameters(x)
    obser <- obs_rates(x)
    nsim <- nrow(x)
    npar <- ncol(param)
    nvar <- ncol(obser)

    cat("Experiment with ", nsim, " simulation"       , s(nsim),
        " of "            , npar, " parameter"        , s(npar),
        " and "           , nvar, " observed variable", s(nvar), "\n", sep = "")
    print_info()
    cat("model parameters:   ", paste(names(param), collapse = ", "),
        "\nobserved variables: ", paste(names(obser), collapse = ", "),
        "\nExperiment overview:\n")

    if (ncol(param) > 2) param2 <- insert_middle(param, nchar, digits)
    else param2 <- param
    if (ncol(obser) > 2) obser2 <- insert_middle(obser, nchar, digits)
    else obser2 <- obser

    y <- cbind(param2,
               obser2,
               x[, c("tmax", "seed"), drop = FALSE],
               "output" = print_output(x)[, "output"])

    if (nrow(y) > 2 * n + interspace) {

      h <- head(y, n)
      t <- tail(y, n)
      hn <- rownames(h)
      tn <- rownames(t)
      m <- setNames(as.data.frame(matrix(".", interspace, ncol(y)),
                                  stringsAsFactors = FALSE), names(y))
      out <- rbind(h, m, t)
      out <- cbind(c(hn, rep(".", interspace), tn), out)
      names(out)[1] <- ""
      print(out, row.names = FALSE)

    } else print(y)

  }
  invisible(x)
}

# [.experiment -----------------------------------------------------------------
#' Extract or Replace Parts of an Experiment Object
#'
#' Extracts or replaces parts of an \code{experiment} object with new value(s).
#'
#' @param i,j,... indices specifying elements to extract or replace. Indices are
#'  numeric or character vectors or empty (missing) or NULL.
#' @param drop boolean, TRUE the result is coerced to the lowest possible
#' dimension.
#'
#' @return  An object of class \code{experiment}
#'
#' @rdname experiment
#' @export
`[.experiment` <- function(exp, i, j, ..., drop = TRUE)
  {
  exp <- as.data.frame(exp)
  exp[i, j, ..., drop = drop]
}


#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
anti_join.experiment <- function(x, y, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class"))) 
 	old_attr <- purrr::keep(old_attr, duplicated(old_attr) == FALSE)
	y <- as.data.frame(y) 
	.data <- dplyr::anti_join(x, y, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
arrange.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::arrange(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
arrange_.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::arrange_(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
arrange_all.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::arrange_all(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
arrange_at.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::arrange_at(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
arrange_if.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::arrange_if(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
distinct.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::distinct(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
distinct_.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::distinct_(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
distinct_all.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::distinct_all(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
distinct_at.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::distinct_at(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
distinct_if.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::distinct_if(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
distinct_prepare.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::distinct_prepare(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
filter.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::filter(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
filter_.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::filter_(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
filter_all.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::filter_all(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
filter_at.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::filter_at(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
filter_if.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::filter_if(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
full_join.experiment <- function(x, y, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class"))) 
 	old_attr <- purrr::keep(old_attr, duplicated(old_attr) == FALSE)
	y <- as.data.frame(y) 
	.data <- dplyr::full_join(x, y, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_by.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_by(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_by_.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_by_(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_by_all.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_by_all(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_by_at.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_by_at(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_by_drop_default.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_by_drop_default(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_by_if.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_by_if(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_by_prepare.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_by_prepare(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_data.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_data(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_indices.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_indices(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_indices_.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_indices_(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_keys.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_keys(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_map.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_map(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_modify.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_modify(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_nest.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_nest(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_rows.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_rows(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_size.experiment <- function(x, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::group_size(x, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_split.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_split(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_trim.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_trim(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_vars.experiment <- function(x, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::group_vars(x, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_walk.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_walk(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
grouped_df.experiment <- function(data, ...) { 
	old_attr <- purrr::keep(attributes(data), names(attributes(data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	data <- as.data.frame(data) 
	.data <- dplyr::grouped_df(data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
groups.experiment <- function(x, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::groups(x, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
inner_join.experiment <- function(x, y, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class"))) 
 	old_attr <- purrr::keep(old_attr, duplicated(old_attr) == FALSE)
	y <- as.data.frame(y) 
	.data <- dplyr::inner_join(x, y, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
is_grouped_df.experiment <- function(x, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::is_grouped_df(x, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
is.grouped_df.experiment <- function(x, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::is.grouped_df(x, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
left_join.experiment <- function(x, y, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class"))) 
 	old_attr <- purrr::keep(old_attr, duplicated(old_attr) == FALSE)
	y <- as.data.frame(y) 
	.data <- dplyr::left_join(x, y, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
mutate.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::mutate(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
mutate_.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::mutate_(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
mutate_all.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::mutate_all(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
mutate_at.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::mutate_at(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
mutate_each.experiment <- function(tbl, ...) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::mutate_each(tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
mutate_each_.experiment <- function(tbl, ...) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::mutate_each_(tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
mutate_if.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::mutate_if(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
n_groups.experiment <- function(x, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::n_groups(x, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
nest_join.experiment <- function(x, y, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class"))) 
 	old_attr <- purrr::keep(old_attr, duplicated(old_attr) == FALSE)
	y <- as.data.frame(y) 
	.data <- dplyr::nest_join(x, y, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
new_grouped_df.experiment <- function(x, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::new_grouped_df(x, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
right_join.experiment <- function(x, y, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class"))) 
 	old_attr <- purrr::keep(old_attr, duplicated(old_attr) == FALSE)
	y <- as.data.frame(y) 
	.data <- dplyr::right_join(x, y, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
sample_frac.experiment <- function(tbl, ...) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::sample_frac(tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
sample_n.experiment <- function(tbl, ...) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::sample_n(tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
select.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::select(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
select_.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::select_(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
select_all.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::select_all(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
select_at.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::select_at(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
select_if.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::select_if(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
semi_join.experiment <- function(x, y, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class"))) 
 	old_attr <- purrr::keep(old_attr, duplicated(old_attr) == FALSE)
	y <- as.data.frame(y) 
	.data <- dplyr::semi_join(x, y, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
slice.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::slice(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
slice_.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::slice_(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
summarise.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::summarise(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
summarise_.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::summarise_(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
summarise_all.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::summarise_all(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
summarise_at.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::summarise_at(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
summarise_each.experiment <- function(tbl, ...) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::summarise_each(tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
summarise_each_.experiment <- function(tbl, ...) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::summarise_each_(tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
summarise_if.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::summarise_if(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
tbl_nongroup_vars.experiment <- function(x, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::tbl_nongroup_vars(x, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
transmute.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::transmute(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
transmute_.experiment <- function(.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::transmute_(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
transmute_all.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::transmute_all(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
transmute_at.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::transmute_at(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
transmute_if.experiment <- function(.tbl, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::transmute_if(.tbl, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
ungroup.experiment <- function(x, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::ungroup(x, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
validate_grouped_df.experiment <- function(x, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::validate_grouped_df(x, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

register_all_s3_methods <-  function() {
	 register_s3_method("dplyr", "anti_join", "experiment")
	 register_s3_method("dplyr", "arrange", "experiment")
	 register_s3_method("dplyr", "arrange_", "experiment")
	 register_s3_method("dplyr", "arrange_all", "experiment")
	 register_s3_method("dplyr", "arrange_at", "experiment")
	 register_s3_method("dplyr", "arrange_if", "experiment")
	 register_s3_method("dplyr", "distinct", "experiment")
	 register_s3_method("dplyr", "distinct_", "experiment")
	 register_s3_method("dplyr", "distinct_all", "experiment")
	 register_s3_method("dplyr", "distinct_at", "experiment")
	 register_s3_method("dplyr", "distinct_if", "experiment")
	 register_s3_method("dplyr", "distinct_prepare", "experiment")
	 register_s3_method("dplyr", "filter", "experiment")
	 register_s3_method("dplyr", "filter_", "experiment")
	 register_s3_method("dplyr", "filter_all", "experiment")
	 register_s3_method("dplyr", "filter_at", "experiment")
	 register_s3_method("dplyr", "filter_if", "experiment")
	 register_s3_method("dplyr", "full_join", "experiment")
	 register_s3_method("dplyr", "group_by", "experiment")
	 register_s3_method("dplyr", "group_by_", "experiment")
	 register_s3_method("dplyr", "group_by_all", "experiment")
	 register_s3_method("dplyr", "group_by_at", "experiment")
	 register_s3_method("dplyr", "group_by_drop_default", "experiment")
	 register_s3_method("dplyr", "group_by_if", "experiment")
	 register_s3_method("dplyr", "group_by_prepare", "experiment")
	 register_s3_method("dplyr", "group_data", "experiment")
	 register_s3_method("dplyr", "group_indices", "experiment")
	 register_s3_method("dplyr", "group_indices_", "experiment")
	 register_s3_method("dplyr", "group_keys", "experiment")
	 register_s3_method("dplyr", "group_map", "experiment")
	 register_s3_method("dplyr", "group_modify", "experiment")
	 register_s3_method("dplyr", "group_nest", "experiment")
	 register_s3_method("dplyr", "group_rows", "experiment")
	 register_s3_method("dplyr", "group_size", "experiment")
	 register_s3_method("dplyr", "group_split", "experiment")
	 register_s3_method("dplyr", "group_trim", "experiment")
	 register_s3_method("dplyr", "group_vars", "experiment")
	 register_s3_method("dplyr", "group_walk", "experiment")
	 register_s3_method("dplyr", "grouped_df", "experiment")
	 register_s3_method("dplyr", "groups", "experiment")
	 register_s3_method("dplyr", "inner_join", "experiment")
	 register_s3_method("dplyr", "is_grouped_df", "experiment")
	 register_s3_method("dplyr", "is.grouped_df", "experiment")
	 register_s3_method("dplyr", "left_join", "experiment")
	 register_s3_method("dplyr", "mutate", "experiment")
	 register_s3_method("dplyr", "mutate_", "experiment")
	 register_s3_method("dplyr", "mutate_all", "experiment")
	 register_s3_method("dplyr", "mutate_at", "experiment")
	 register_s3_method("dplyr", "mutate_each", "experiment")
	 register_s3_method("dplyr", "mutate_each_", "experiment")
	 register_s3_method("dplyr", "mutate_if", "experiment")
	 register_s3_method("dplyr", "n_groups", "experiment")
	 register_s3_method("dplyr", "nest_join", "experiment")
	 register_s3_method("dplyr", "new_grouped_df", "experiment")
	 register_s3_method("dplyr", "right_join", "experiment")
	 register_s3_method("dplyr", "sample_frac", "experiment")
	 register_s3_method("dplyr", "sample_n", "experiment")
	 register_s3_method("dplyr", "select", "experiment")
	 register_s3_method("dplyr", "select_", "experiment")
	 register_s3_method("dplyr", "select_all", "experiment")
	 register_s3_method("dplyr", "select_at", "experiment")
	 register_s3_method("dplyr", "select_if", "experiment")
	 register_s3_method("dplyr", "semi_join", "experiment")
	 register_s3_method("dplyr", "slice", "experiment")
	 register_s3_method("dplyr", "slice_", "experiment")
	 register_s3_method("dplyr", "summarise", "experiment")
	 register_s3_method("dplyr", "summarise_", "experiment")
	 register_s3_method("dplyr", "summarise_all", "experiment")
	 register_s3_method("dplyr", "summarise_at", "experiment")
	 register_s3_method("dplyr", "summarise_each", "experiment")
	 register_s3_method("dplyr", "summarise_each_", "experiment")
	 register_s3_method("dplyr", "summarise_if", "experiment")
	 register_s3_method("dplyr", "tbl_nongroup_vars", "experiment")
	 register_s3_method("dplyr", "transmute", "experiment")
	 register_s3_method("dplyr", "transmute_", "experiment")
	 register_s3_method("dplyr", "transmute_all", "experiment")
	 register_s3_method("dplyr", "transmute_at", "experiment")
	 register_s3_method("dplyr", "transmute_if", "experiment")
	 register_s3_method("dplyr", "ungroup", "experiment")
	 register_s3_method("dplyr", "validate_grouped_df", "experiment")
}


# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R 
# Thu Apr 19 10:53:24 CEST 2018 (adapted 
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

.onLoad <- function(libname, pkgname) {
  register_all_s3_methods()
}

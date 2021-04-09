testthat::test_that("Tests returns error/warning from is.experiment", {
  testthat::expect_error(is.experiment(NULL),
                         "An object `experiment` cannot contain NULL value.")
  sir1 <- load_experiment("sir",
                          system.file("models", "sir.gaml", package = "gamar"))
  sir1$p_S0 <- NA
  testthat::expect_error(is.experiment(sir1),
                         "An object `experiment` cannot contain NA value.")
})

test_that(
  "Tests returns NULL from load_experiment", {
    sir1 <- load_experiment("sir",
                            system.file("models", "sir.gaml",
                                        package = "gamar"))
    sir1[, ] <- NULL
    testthat::expect_equal(gamar:::get_parameters(sir1), NULL)
    testthat::expect_equal(gamar:::get_variables(sir1), NULL)
    testthat::expect_is(gamar:::make_df_dic(gamar:::get_parameters(sir1)),
                               "list")
})

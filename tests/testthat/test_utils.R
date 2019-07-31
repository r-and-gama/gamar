
test_that("Tests `read_gaml_experiment` returns error when necessary", {

  expect_error(
    gamar:::read_gaml_experiment("sir", "test_files/sir_error_read_gaml.gaml"))

})

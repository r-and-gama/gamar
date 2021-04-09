testthat::test_that("Tests returns error/warning from save_to_gama", {
  test <- "test"
  testthat::expect_equal(save_to_gama(test), "Unknown class")
})

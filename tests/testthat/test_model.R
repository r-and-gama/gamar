testthat::test_that("Tests returns error/warning from save_to_gama", {
  test <- "test"
  testthat::expect_equal(model(test), "Unknown class")
  testthat::expect_equal(`model<-`(test, "a"), "Unknown class")
})

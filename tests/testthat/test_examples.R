test_that("Tests package examples", {
  skip_on_travis()
  skip_on_appveyor()
  testthat::test_examples("man/")
})

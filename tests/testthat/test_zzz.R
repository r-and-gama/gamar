test_that("init_gama_jar returns NA if path NA", {
  testthat::expect_equal(gamar:::init_gama_jar(NA), NA)
})

test_that(
  "Tests returns correct output from call_gama", {
    testthat::expect_error(call_gama(NULL, 1),
                           "Gama fails to run your experiment.")

    unlink(tempdir(), TRUE)

    testthat::expect_error(call_gama(NULL, 1, output_dir = tempdir()),
                           "Gama fails to run your experiment.")

  })

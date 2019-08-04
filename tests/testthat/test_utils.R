test_that("Test special character in test_char", {
  expect_error(gamar:::test_schar("'"))
})

test_that("Test requested if experiment exists in gaml", {
  exp <- load_experiment("sir",
                         system.file("models", "sir.gaml", package = "gamar"))
  expect_error(gamar:::check_experiment("abc", model(exp)))
})

})

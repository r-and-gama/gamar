test_that("validate_experiment tests", {

  expect_error(experiment(
                  data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
                  data.frame(S = 1, I = 1, R = -1),
                1000, 1, "sir", system.file("models", "sir.gaml", package = "gamar")))

  expect_error(experiment(
    data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
    data.frame(S = 1, I = 1, R = 1),
    -1000, 1, "sir", system.file("models", "sir.gaml", package = "gamar")))

  expect_error(experiment(
    data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
    data.frame(S = 1, I = 1, R = 1),
    1000, "a", "sir", system.file("models", "sir.gaml", package = "gamar")))

  expect_error(experiment(
    data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
    data.frame(M = 1, I = 1, R = 1),
    1000, 1, "sir", system.file("models", "sir.gaml", package = "gamar")))

})

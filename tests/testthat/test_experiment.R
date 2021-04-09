exp <- experiment(
  data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
  data.frame(S = 1, I = 1, R = 1),
  1000, 1, "sir", system.file("models", "sir.gaml", package = "gamar"))

test_that("validate_experiment tests", {

  expect_error(experiment(
                  data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
                  data.frame(S = 1, I = 1, R = -1), 1000, 1, "sir",
                system.file("models", "sir.gaml", package = "gamar")))

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

  expect_error(model(exp)$info$Parameters[[1]]["name"] <- "abc")
  expect_error(model(exp)$info$Parameters[[1]]["type"] <- "STRING")
  expect_error(model(exp)$info$Outputs[[1]]["name"] <- "abc")
  expect_error(model(exp)$md5sum <- "abc")

})

test_that("Test output not NA in new_experiment", {
  expect_error(gamar:::new_experiment(
    data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
    data.frame(S = 1, I = 1, R = 1),
    1000, 1, "sir", system.file("models", "sir.gaml", package = "gamar"),
    output = "abc"))

  expect_error(gamar:::new_experiment(
    data.frame(S0 = 999),
    data.frame(S = 1, I = 1, R = 1),
    1000, 1, "sir", system.file("models", "sir.gaml", package = "gamar"),
    output = "abc"))
  expect_error(gamar:::new_experiment(
    parameters = data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
    obsrates = NULL,
    tmax = 1000, seed = 1, experiment = "sir",
    model = system.file("models", "sir.gaml", package = "gamar"),
    output = "abc"))
})

test_that("experiment and new_experiment", {
  dic <- c("name" = "abc")
  expect_error(experiment(
                  parameters = data.frame(S0 = 999, I0 = 1, R0 = 0,
                            beta = 1.5, gamma = .15),
                  tmax = 1000, seed = 1, experiment = "sir",
                  model = system.file("models", "sir.gaml", package = "gamar"),
                  dic = dic))
  expect_s3_class(experiment(
                    parameters = data.frame(S0 = 999, I0 = 1, R0 = 0,
                                            beta = 1.5, gamma = .15),
                    obsrates = NULL,
                    tmax = 1000, seed = 1, experiment = "sir",
                    model = system.file("models", "sir.gaml",
                                        package = "gamar")),
                  "experiment")

  expect_s3_class(experiment(
                    parameters = NULL,
                    obsrates = data.frame(S = 1, I = 1, R = 1),
                    tmax = 1000, seed = 1, experiment = "sir",
                    model = system.file("models", "sir.gaml",
                                        package = "gamar")),
                  "experiment")

  expect_s3_class(experiment(
                    parameters = NULL,
                    obsrates = NULL,
                    tmax = 1000, seed = 1, experiment = "sir",
                    model = system.file("models", "sir.gaml",
                                        package = "gamar")),
                  "experiment")
  expect_s3_class(gamar:::new_experiment(
                    parameters = NULL,
                    obsrates = NULL,
                    tmax = 1000, seed = 1, experiment = "sir",
                    model = system.file("models", "sir.gaml", package = "gamar"),
                    output = "abc"),
                  "experiment")
})

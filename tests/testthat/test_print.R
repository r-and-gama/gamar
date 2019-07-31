testthat::test_that(
  "Tests returns correct output from run_experiment", {

    df <-   data.frame(I0 = 100,
                       S0 = 900,
                       R0 = 0,
                       beta = 1.5,
                       gamma = .15,
                       I = 2,
                       S = 1,
                       R = 5,
                       nbiter = 10,
                       seed = "123456789")

  # Check realexp
  exp0 <- as_experiment(df, parameters = c("I0", "S0", "R0", "beta", "gamma"),
                        obsrates  = c("I", "S", "R"),
                        tmax = "nbiter", seed = "seed", experiment = "sir",
                        model =
                          system.file("models", "sir.gaml", package = "rama"))

  exp1 <- exp0
  exp1[,] <- NULL
  testthat::expect_equal(capture.output(exp1)[1],
                         "Experiment without any simulation, tunable parameter or observed variable")

  testthat::expect_length(capture.output(repl(exp0, 100)), 22)

  exp3 <- as_experiment(df, parameters = c("I0", "S0"),
                        obsrates  = c("I", "S"),
                        tmax = "nbiter", seed = "seed", experiment = "sir",
                        model =
                          system.file("models", "sir.gaml", package = "rama"))

  testthat::expect_length(capture.output(exp3), 8)
  testthat::expect_length(capture.output(exp0), 8)

})

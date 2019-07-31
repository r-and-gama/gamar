testthat::test_that(
  "Tests returns correct output from run_experiment", {

    df <-   data.frame(S0 = 900,
                       I0 = 100,
                       R0 = 0,
                       beta = 1.5,
                       gamma = .15,
                       S = 1,
                       I = 2,
                       R = 5,
                       nbiter = 10,
                       seed = "123456789")

    # Check error
    testthat::expect_error(run_experiment(df),
                           "The argument \"exp\" is not an object of class \"experiment\".")

    # Check realexp
    exp0 <- as_experiment(df, parameters = c("S0", "I0", "R0", "beta", "gamma"),
                          obsrates  = c("S", "I", "R"),
                          tmax = "nbiter", seed = "seed", experiment = "sir",
                          model =
                            system.file("models", "sir.gaml", package = "rama"))
    otp <- run_experiment(exp0)
    testthat::expect_equal(otp$r_I, 2)

    # check display
    exp1 <- load_experiment("prey_predator",
                            system.file("models/predator_prey/models",
                                        "predator_prey.gaml", package = "rama"))

    exp1$tmax <- 2L
    test1 <- run_experiment(exp1, hpc = 2, display = TRUE)
    testthat::expect_equal(test1$tmax, 2)

    test2 <- run_experiment(exp1, hpc = 2, save = TRUE)
    testthat::expect_equal(dim(test1$output[[1]]), c(2, 6))

  })

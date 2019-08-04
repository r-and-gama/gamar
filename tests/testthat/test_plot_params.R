exp <- load_experiment("sir",
                       system.file("models", "sir.gaml", package = "gamar"))
df0 <-   data.frame(S0 = c(900, 900, 900),
                   I0 = c(100, 100, 100),
                   R0 = 0,
                   beta = c(1.4, 1.5, 1.6),
                   gamma = .15,
                   S = c(1, 2, 3),
                   I = c(2, 4, 6),
                   R = c(10, 20, 30),
                   nbiter = 1000,
                   seed = 123456789)
exp0 <- as_experiment(df0, parameters = c("S0", "I0", "R0", "beta", "gamma"),
                      obsrates  = c("S", "I", "R"),
                      tmax = "nbiter", seed = "seed", experiment = "sir",
                      model =
                      system.file("models", "sir.gaml", package = "gamar"))


df1 <-   data.frame(S0 = c(900, 900, 900),
                    I0 = c(100, 100, 100),
                    R0 = 0,
                    beta = c(1.4, 1.5, 1.6),
                    gamma = .15,
                    S = c(1, 2, 3),
                    I = c(2, 4, 6),
                    R = c(10, 20, 30),
                    nbiter = 1000,
                    seed = 123456789)
exp1 <- as_experiment(df1, parameters = c("S0", "I0", "R0", "beta", "gamma"),
                      obsrates  = c("S", "I", "R"),
                      tmax = "nbiter", seed = "seed", experiment = "sir",
                      model =
                        system.file("models", "sir.gaml", package = "gamar"))

df2 <-   data.frame(S0 = c(900, 300, 500),
                    I0 = c(100, 100, 100),
                    R0 = 0,
                    beta = c(1.4, 1.5, 1.6),
                    gamma = .15,
                    S = c(1, 2, 3),
                    I = c(2, 4, 6),
                    R = c(10, 20, 30),
                    nbiter = 1000,
                    seed = 123456789)
exp2 <- as_experiment(df2, parameters = c("S0", "I0", "R0", "beta", "gamma"),
                      obsrates  = c("S", "I", "R"),
                      tmax = "nbiter", seed = "seed", experiment = "sir",
                      model =
                        system.file("models", "sir.gaml", package = "gamar"))


test_that("Test valid exp to plot", {
  expect_error(plot_params(exp))
  expect_error(plot_params(repl(exp, 3)))
  exp[,] <- NULL
  expect_error(plot_params(exp))

})

test_that("Test n = 1", {
  expect_type(plot_params(exp1), "integer")
})
test_that("Test n = 2", {
  expect_type(plot_params(exp2), "integer")
})

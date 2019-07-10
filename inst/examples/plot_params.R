df <-   data.frame(S0 = c(900, 800, 500), # this is a data frame of 3 lines
                  I0 = c(100, 200, 500),
                  R0 = 0,
                  beta = c(1.4, 1.5, 1.6),
                  gamma = .15,
                  S = c(1, 2, 3),
                  I = c(2, 4, 6),
                  R = c(10, 20, 30),
                  nbiter = 1000,
                  seed = 123456789)
exp0 <- as_experiment(df, parameters = c("S0", "I0", "R0", "beta", "gamma"),
                      obsrates  = c("S", "I", "R"),
                      tmax = "nbiter", seed = "seed", experiment = "sir",
                      model =
                       system.file("models", "sir.gaml", package = "gamar"))
plot_params(exp0)

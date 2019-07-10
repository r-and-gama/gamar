## experiment ##
exp0 <- experiment(
  data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
  data.frame(S = 1, I = 1, R = 1),
  1000, 1, "sir", system.file("models", "sir.gaml", package = "gamar"))
print(exp0)

## is.experiment ##
# to test if an object is a class `experiment`
df <- data.frame("S0" = rep(999, 5), "I0" = rep(1, 5), "R0" = rep(0, 5),
                 "beta" = rep(1.5, 5), "gamma" = runif (5, 0, 1),
                 "S" = rep(1, 5), "I" = rep(1, 5), "R" = rep(1, 5),
                 "a" = rep(1000, 5), "b" = rep(1, 5))
exp <- as_experiment(df, parameters = c("S0", "I0", "R0", "beta", "gamma"),
                     obsrates = c("S", "I", "R"),
                     tmax = "a",
                     seed = "b",
                     experiment = "sir",
                     model =
                       system.file("models", "sir.gaml", package = "gamar"))
is.experiment(exp)



# Here is an experiment with 1 simulation:
sir1 <- load_experiment("sir",
                        system.file("models", "sir.gaml", package = "gamar"))
sir1

sir2 <- sir1
# If  we now replace the values of "p_S0" of "sir2" by a single value:
sir2$p_S0 <- 2
sir2
# If you wish to delete one column:
sir2$r_R <- NULL
sir2

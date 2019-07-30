# First situation: the sets of names of the data frame and the experiment
# are exactly the same:
sir1 <- load_experiment("sir",
                        system.file("models", "sir.gaml", package = "gamar"))
exp <- sir1
df <- as.data.frame(repl(sir1, 3))
map_experiment(df, exp)
# Second situation: the names of the data frame are included in the names of
# the experiment AND the numbers of rows of the data frame and the experiment
# are equal:
exp <- repl(sir1, 3)
df <- as.data.frame(exp)[, c(1, 3, 6)]
map_experiment(df, exp)

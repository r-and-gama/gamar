# A first example:
if (exists("sir1", inherits = FALSE)) rm(sir1)
df <- as.data.frame(repl(sir1, 5))
exp_name <- name(sir1)
gaml_file <- attr(sir1, "model")$path
as_experiment(df, experiment = exp_name, model = gaml_file)
# Alternative uses:
as_experiment(df, 1:5, 6:8, 9, 10, exp_name, gaml_file)
as_experiment(df, c("p_S0", "p_I0", "p_R0", "p_beta", "p_gamma"),
              c("r_S", "r_I", "r_R"), "tmax", "seed", exp_name, gaml_file)
# Or a mixture of character and numeric indexes:
as_experiment(df, 1:5, c("r_S", "r_I", "r_R"), "tmax", "seed", exp_name,
              gaml_file)
# And even using default parameters specification:
as_experiment(df, obsrates = c("r_S", "r_I", "r_R"),
              experiment = exp_name, model = gaml_file)

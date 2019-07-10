#load experiment
gaml_file <- system.file("models", "sir.gaml", package = "gamar")
exp1 <- load_experiment("sir", gaml_file)
# run experiment
out <- run_experiment(exp1, hpc = 2)

# for a more complexe example
exp1 <- repl(exp1, 2)
exp1$r_R <- 2L:3L
exp1$tmax <- 2L
output <- run_experiment(exp1, hpc = 2)
output

# to save the experiment input and output (by default in the working directory)
output <- run_experiment(exp1, hpc = 2, save = TRUE)

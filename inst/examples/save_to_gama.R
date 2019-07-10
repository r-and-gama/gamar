gaml_file <- system.file("models", "sir.gaml", package = "gamar")
exp1 <- load_experiment("sir", gaml_file)
save_to_gama(exp1)

exp1 <- load_experiment("sir",
                        system.file("models", "sir.gaml", package = "gamar"))
exp2 <- repl(exp1, 10)
parameters(exp2)

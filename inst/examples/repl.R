exp <- load_experiment("sir",
                       system.file("models", "sir.gaml", package = "gamar"))
exp1 <- repl(exp, times = 4)
exp1$seed <- c(1:4)
repl(exp1, times = c("1" = 3, "4" = 5))

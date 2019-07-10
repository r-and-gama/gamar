sir1 <- load_experiment("sir",
                        system.file("models", "sir.gaml", package = "gamar"))
# 1. First type of use: tranforming an experiment into one with a full
# factorial design: (use the function `repl`, if you want more details `?repl`)
sir2 <- repl(sir1, 3)
sir2$p_S0 <- 1:3
sir2
sir2[1, 2] <- 2
# "sir2" is not full factorial:
sir2
# this is:
fullfact(sir2)

# 2. Second type of use: by providing vectors of values to overwrite elements
# of the "experiment" object and then expand it into full factorial design:
fullfact(sir2, p_S0 = 1:3, p_I0 = 4:5)

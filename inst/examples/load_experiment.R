# Listing the models available in the "examples" directory of the "gamar" library:
grep(".gaml", dir(system.file("models", package = "gamar")), value = TRUE)
# Loading experiment "sir" from the "sir.gaml" file:
exp1 <- load_experiment("sir",
                        system.file("models", "sir.gaml", package = "gamar"))
# Checking the class:
class(exp1)

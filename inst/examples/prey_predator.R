# Load an experiment
exp <- load_experiment("prey_predator",
                       system.file("models/predator_prey/models",
                                   "predator_prey.gaml", package = "gamar"))

# to explore the parameters and the observed variables
parameters(exp)
obs_rates(exp)

# to set the experiment
exp$p_Initial_number_of_preys_ <- 990L
exp$p_Initial_number_of_predators_ <- 100L
exp$p_Predator_probability_reproduce_ <- 0.1
exp$tmax <- 100L

# to run the experiment
out <- run_experiment(exp, hpc = 2, display = TRUE)

# to visualize the output
with(out$output[[1]],
    plot(Step, `r_Number_of_preys`, type = "l", lwd = 2, col = "red"))

# to make a movie
path_movie <- make_movie(out, "r_main_display")

# to print the result, it is necessary to have the package `magick`
magick::image_read(path_movie)

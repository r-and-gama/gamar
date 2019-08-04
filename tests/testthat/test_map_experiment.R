exp <- load_experiment("sir",
                       system.file("models", "sir.gaml", package = "gamar"))

df <- as.data.frame(exp)[, c(1, 3, 6)]

test_that("Test obsrates converted into integers", {

  df$r_S <- "1"
  expect_message(map_experiment(df, exp))
})

test_that("Invalide df", {
  colnames(df) <- c("p_S0", "p_R0", "abc")
  expect_error(map_experiment(df, exp))

})

test_that("Test missing parameter", {
  sir1 <- load_experiment("sir",
                          system.file("models", "sir.gaml", package = "gamar"))
  df <- as.data.frame(repl(sir1, 5))

  expect_error(as_experiment(df, experiment = name(sir1)))
  expect_error(as_experiment(df, model = attr(sir1, "model")$path))

} )

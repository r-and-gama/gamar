exp <- load_experiment("sir",
                       system.file("models", "sir.gaml", package = "gamar"))

test_that("Test unknow class", {
  expect_equal(repl(NULL), "Unknown class")
})

test_that("Test times=NULL", {
  expect_equal(repl(exp), exp)

})

test_that("Test warning requested_row", {
  times <- c("3" = 10)
  expect_warning(repl(exp, times))
})

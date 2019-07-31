test_that("Test file-not-exist error", {
  expect_error(list_experiments("file_not_exist.gaml"))
})

test_that("Test if no experiment is indicated in gaml file", {
  expect_null(list_experiments("test_files/sir_no_exp.gaml"))
})

test_that("Test gaml without type", {
  test1 <- list_experiments("test_files/sir_no_type.gaml")
  expect_equal(test1$type, "gui")
})

test_that("Tests names_of_left_and_right", {
  expect_type(gamar:::names_of_left_and_right(c("abcd", "xayz"), 5), "list")
  expect_type(gamar:::names_of_left_and_right(c("acbd", "dcaq", "dcqa"), 10),
              "list")
})

test_that("Tests insert_middle", {
  df <- data.frame("acbd" = c(1, 2), "dcaq" = c(3, 4), "dcqa" = c(5, 6))
  expect_type(gamar:::insert_middle(df, 10), "list" )
})

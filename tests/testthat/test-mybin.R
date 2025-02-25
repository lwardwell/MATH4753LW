test_that("mybin works", {
  l <- mybin(iter = 4)
  expect_length(l,11)
  expect_visible(mybin(iter = 3))
  expect_setequal(l>=0, rep(TRUE,11))
})

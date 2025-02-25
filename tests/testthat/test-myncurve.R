test_that("myncurve works correctly", {
  # Test that the function returns the correct values
  result <- myncurve(5, 2)

  # Test return value structure
  expect_type(result, "list")
  expect_named(result, c("mu", "sigma"))

  # Test that the values are correctly stored in the list
  expect_equal(result$mu, 5)
  expect_equal(result$sigma, 2)

  # Test that a plot is created (this is tricky but we can check if plotting occurred)
  # We can use a special device to check if plotting commands were issued
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(myncurve(10, 3))

  # Test error handling (if sigma is negative or zero)
  expect_error(myncurve(0, -1))
  expect_error(myncurve(0, 0))
})

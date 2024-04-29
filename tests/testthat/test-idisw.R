library(testthat)

# Test Cases
test_that("idisu function calculates IDISU correctly", {
  # Create sample data
  ## Non-ordered
  set.seed(123)
  pop <- round(rnorm(25, 1000, 300))
  est <- rbinom(25, 1000, .065)
  se <- rbinom(25, 100, .045)
  ordered <- rep(0, 25)
  fav <- rep(1, 25)
  scale <- rep(100, 25)
  ref <- rep(0, 25)
  ref[sample(25, 1)] <- 1

  # Call the idisw function
  result <- idisw(pop = pop,
                est = est,
                se = se,
                scaleval = scale)

  # Expected values
  expected_idisu <- 8.6178417
  expected_se <- NA

  # Compare the calculated values with the expected values
  expect_equal(result$estimate, expected_idisu, tolerance = 1e-4)
  expect_equal(result$se, expected_se, tolerance = 1e-4)
})

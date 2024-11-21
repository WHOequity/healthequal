library(testthat)

# Test Cases
test_that("parisk function calculates parisk correctly", {
  # Create sample data
  ## Non-ordered
  set.seed(123)
  pop <- round(rnorm(25, 1000, 300))
  est <- rbinom(25, 1000, .065)
  se <- rbinom(25, 100, .045)
  ordered <- rep(0, 25)
  fav <- rep(1, 25)
  scale <- rep(100, 25)
  ref <- rep(0,25)
  ref[sample(25,1)] <- 1

  # Call the parisk function
  result <- parisk(pop = pop,
                est = est,
                se = se,
                scaleval = scale,
                ordered_dimension = ordered,
                favourable_indicator = fav)

  # Expected values
  expected_parisk <- 16.293152
  expected_se <- 1.1994447
  expected_lowerci <- 13.94224
  expected_upperci <- 18.644063

  # Compare the calculated values with the expected values
  expect_equal(result$estimate, expected_parisk, tolerance = 1e-4)
  expect_equal(result$se, expected_se, tolerance = 1e-4)
  expect_equal(result$lowerci, expected_lowerci, tolerance = 1e-4)
  expect_equal(result$upperci, expected_upperci, tolerance = 1e-4)
})

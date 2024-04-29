library(testthat)

# Test Cases
test_that("ti function calculates TI correctly", {
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

  # Call the ti function
  result <- ti(pop = pop,
                est = est,
                se = se)

  # Expected values
  expected_ti <- 5.0359201
  expected_se <- .0014201
  expected_lowerci <- 5.0331367
  expected_upperci <- 5.0387035

  # Compare the calculated values with the expected values
  expect_equal(result$estimate, expected_ti, tolerance = 1e-4)
  expect_equal(result$se, expected_se, tolerance = 1e-4)
  expect_equal(result$lowerci, expected_lowerci, tolerance = 1e-4)
  expect_equal(result$upperci, expected_upperci, tolerance = 1e-4)
})

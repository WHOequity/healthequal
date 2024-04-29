library(testthat)

# Test Cases
test_that("bgv function calculates BGV correctly", {
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

  # Call the aci function
  result <- bgv(pop = pop,
                est = est,
                se = se)

  # Expected values
  expected_bgv <- 46.198326
  expected_se <- 17.134647
  expected_lowerci <- expected_bgv - expected_se * qnorm(.975)
  expected_upperci <- expected_bgv + expected_se * qnorm(.975)

  # Compare the calculated values with the expected values
  expect_equal(result$estimate, expected_bgv, tolerance = 1e-6)
  expect_equal(result$se, expected_se, tolerance = 1e-6)
  expect_equal(result$lowerci, expected_lowerci, tolerance = 1e-6)
  expect_equal(result$upperci, expected_upperci, tolerance = 1e-6)
})

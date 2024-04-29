library(testthat)

# Test Cases
test_that("mld function calculates MLD correctly", {
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
  result <- mld(pop = pop,
                est = est,
                se = se)

  # Expected values
  expected_mld <- 5.0524006
  expected_se <- .0014242
  expected_lowerci <- expected_mld - expected_se * qnorm(.975)
  expected_upperci <- expected_mld + expected_se * qnorm(.975)

  # Compare the calculated values with the expected values
  expect_equal(result$estimate, expected_mld, tolerance = 1e-3)
  expect_equal(result$se, expected_se, tolerance = 1e-3)
  expect_equal(result$lowerci, expected_lowerci, tolerance = 1e-3)
  expect_equal(result$upperci, expected_upperci, tolerance = 1e-3)
})

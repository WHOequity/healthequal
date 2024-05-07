library(testthat)

# Test Cases
test_that("sii function calculates SII correctly", {
  # Create sample data
  ## Ordered
  set.seed(123)
  pop <- round(rnorm(5, 100, 40))
  est <- c(20, 30, 40, 50, 60)
  se <- c(5, 7, 10, 12, 14)
  subgroup_order <- c(1, 2, 3, 4, 5)
  ordered <- rep(1, 5)
  scale <- rep(100, 5)
  fav <- rep(1, 5)

  # Call the aci function
  result <- sii(pop = pop,
                est = est,
                subgroup_order = subgroup_order,
                scaleval = scale)

  # Expected values
  expected_sii <- 44.44502
  expected_se <- 0.02190822
  expected_lowerci <- 40.15109
  expected_upperci <- 48.73895

  # Compare the calculated values with the expected values
  expect_equal(result$estimate, expected_sii, tolerance = 1e-6)
  expect_equal(result$se, expected_se, tolerance = 1e-6)
  expect_equal(result$lowerci, expected_lowerci, tolerance = 1e-6)
  expect_equal(result$upperci, expected_upperci, tolerance = 1e-6)
})

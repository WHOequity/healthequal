library(testthat)

# Test Cases
test_that("paf function calculates paf correctly", {
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

  # Call the paf function
  result <- paf(pop = pop,
                est = est,
                se = se,
                scaleval = scale,
                ordered_dimension = ordered,
                favourable_indicator = fav)

  # Expected values
  expected_paf <- 24.06426
  expected_se <- .01771526
  expected_lowerci <- 24.029539
  expected_upperci <- 24.098982

  # Compare the calculated values with the expected values
  expect_equal(result$estimate, expected_paf, tolerance = 1e-4)
  expect_equal(result$se, expected_se, tolerance = 1e-4)
  expect_equal(result$lowerci, expected_lowerci, tolerance = 1e-6)
  expect_equal(result$upperci, expected_upperci, tolerance = 1e-6)
})

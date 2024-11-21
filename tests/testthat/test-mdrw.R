library(testthat)

# Test Cases
test_that("mdrw function calculates mdrw correctly", {
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

  # Call the mdrw function
  result <- mdrw(pop = pop,
                est = est,
                se = se,
                scaleval = scale,
                favourable_indicator = fav,
                reference_subgroup = ref)

  # Expected values
  expected_mdrw <- 7.2228465

  # Compare the calculated values with the expected values
  expect_equal(result$estimate, expected_mdrw, tolerance = 1e-4)
})

library(testthat)

# Test Cases
test_that("parisk function calculates PAR correctly", {
  # Create sample data
  ## Ordered
  set.seed(123)
  pop <- round(rnorm(5, 100, 40))
  est <- c(20, 30, 40, 50, 60)
  se <- c(5, 7, 10, 12, 14)
  subgroup_order <- c(1, 2, 3, 4, 5)
  ordered <- rep(1, 5)
  fav <- rep(1, 5)
  scale <- rep(100, 5)

  # Call the parisk function
  result <- parisk(pop = pop,
                est = est,
                subgroup_order = subgroup_order,
                favourable_indicator = fav,
                ordered_dimension = ordered,
                scaleval = scale)

  # Expected values
  expected_parisk <- 18.775509
  expected_se <- 4.4066348
  expected_lowerci <- 10.138505
  expected_upperci <- 27.412513

  # Compare the calculated values with the expected values
  expect_equal(result$estimate, expected_parisk, tolerance = 1e-2)
  expect_equal(result$se, expected_se, tolerance = 1e-2)
  expect_equal(result$lowerci, expected_lowerci, tolerance = 1e-2)
  expect_equal(result$upperci, expected_upperci, tolerance = 1e-2)
})

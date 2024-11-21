library(testthat)

# Test Cases
test_that("rci function calculates RCI correctly", {
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

  # Call the rci function
  result <- rci(pop = pop,
                est = est,
                subgroup_order = subgroup_order)

  # Expected values
  expected_rci <- .17776859
  expected_se <- .0071240491
  expected_lowerci <- 0.16380545
  expected_upperci <- 0.19173172

  # Compare the calculated values with the expected values
  expect_equal(result$estimate, expected_rci, tolerance = 1e-3)
  expect_equal(result$se, expected_se, tolerance = 1e-3)
  expect_equal(result$lowerci, expected_lowerci, tolerance = 1e-3)
  expect_equal(result$upperci, expected_upperci, tolerance = 1e-3)
})

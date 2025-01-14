library(testthat)

test_that("input validation works", {
  expect_error(interest("7%", 1000, 10))
  expect_error(interest(0.07, -1000, 10))
  expect_error(interest(0.07, 1000, 0.5))
})

test_that("basic calculations are correct", {
  # Test with 100% return rate for 1 year
  expect_equal(interest(1, 100, 1), 200)

  # Test with 0% return rate
  expect_equal(interest(0, 100, 10), 100)

  # Test with negative return rate (decay)
  expect_equal(interest(-0.5, 100, 1), 50)
})

test_that("compound interest calculations are accurate", {
  # Known compound interest scenario: 7% for 2 years
  expect_equal(interest(0.07, 100, 2), 100 * (1.07)^2)

  # Test precision with small rates
  expect_equal(interest(0.001, 10000, 5), 10000 * (1.001)^5)
})

test_that("edge cases are handled", {
  # Very small numbers
  expect_gt(interest(0.0001, 0.01, 1000), 0)

  # Extreme decay
  expect_gt(interest(-0.99, 1000, 10), 0)
})

test_that("function preserves precision", {
  result <- interest(0.0725, 10000, 30)
  expect_equal(round(result, 2), round(10000 * (1.0725)^30, 2))
})

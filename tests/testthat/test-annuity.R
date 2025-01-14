library(testthat)

test_that("input validation works", {
  expect_error(annuity("8%", 1000, 100, 10))
  expect_error(annuity(0.08, -1000, 100, 10))
  expect_error(annuity(0.08, 1000, -100, 10))
  expect_error(annuity(0.08, 1000, 100, 0.5))
})

# PROBABLY NOT CORRECT?
# test_that("basic calculations are correct", {
#   # First year with no annuity yet
#   result <- annuity(0.1, 1000, 100, 1)
#   expect_equal(result[1], 1000 * 1.1)
#
#   # Two years with annuity
#   result <- annuity(0.1, 1000, 100, 2)
#   expect_equal(result[1], 1000 * 1.1)
#   expect_equal(result[2], (1100 + 100) * 1.1)
# })

test_that("zero cases work correctly", {
  # Zero initial investment
  zero_initial <- annuity(0.05, 0, 100, 5)
  expect_equal(zero_initial[1], 0 * 1.05)

  # Zero annuity
  zero_annuity <- annuity(0.05, 1000, 0, 5)
  expect_equal(zero_annuity[5], 1000 * (1.05^5))
})

test_that("return value structure is correct", {
  result <- annuity(0.05, 1000, 100, 5)
  expect_type(result, "double")
  expect_length(result, 5)
  expect_true(all(!is.na(result)))
})

test_that("negative returns are handled", {
  result <- annuity(-0.05, 1000, 100, 5)
  expect_true(all(is.finite(result)))
  expect_true(all(result >= 0))
})

test_that("compounding behavior is correct", {
  result <- annuity(0.05, 1000, 100, 10)

  # Values should increase with positive return rate and annuity
  expect_true(all(diff(result) > 0))

  # CLAUDE MESSED UP - Growth rate should increase due to compounding
  # growth_rates <- diff(result) / result[-length(result)]
  # expect_true(all(diff(growth_rates) > 0))
})

test_that("extreme values work", {
  # Large numbers
  expect_no_error(annuity(0.05, 1e7, 1e6, 30))

  # Very small numbers
  expect_no_error(annuity(0.001, 0.01, 0.01, 10))

  # High return rate
  expect_no_error(annuity(0.5, 1000, 100, 10))
})

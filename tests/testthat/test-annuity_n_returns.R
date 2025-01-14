library(testthat)
library(ggplot2)

# First, let's create a helper function to calculate the expected investment value
# This gives us a way to verify the function's calculations
# calculate_expected_value <- function(initial, annuity, rate, years) {
#   investment <- initial
#   add_yearly <- annuity
#   annuity    <- 0
#   for(i in 1:years) {
#     investment <- (investment + annuity) * (1 + rate)
#     annuity    <- add_yearly # You have to have zero annuity on the first year. Then you can start adding the annuities in each of the years following.
#   }
#   return(investment)
# }

test_that("input validation works correctly", {
  # Test invalid inputs
  expect_error(annuity_n_returns(-1000, 100, 10, c(0.06, 0.07)))
  expect_error(annuity_n_returns(1000, -100, 10, c(0.06, 0.07)))
  expect_error(annuity_n_returns(1000, 100, 0.5, c(0.06, 0.07)))
  expect_error(annuity_n_returns(1000, 100, 10, NA))

  # Test valid inputs produce no errors
  expect_no_error(annuity_n_returns(1000, 100, 10, c(0.06, 0.07)))
})

test_that("financial calculations are accurate", {
  # Test with simple values for easy manual verification
  result <- annuity_n_returns(1000, 100, 2, c(0.10))
  data_table <- result[[2]]

  # Calculate expected values
  expected_year1 <- (1000 + 0) * (1 + 0.10)  # First year: initial investment only
  expected_year2 <- (expected_year1 + 100) * (1 + 0.10)  # Second year: previous amount plus annuity

  # Compare with actual results
  expect_equal(data_table[1, "0.1"], expected_year1, tolerance = 0.01)
  expect_equal(data_table[2, "0.1"], expected_year2, tolerance = 0.01)

  # Test with multiple rates
  multi_rate_result <- annuity_n_returns(1000, 100, 1, c(0.05, 0.10))
  expect_equal(multi_rate_result[[2]][1, "0.05"], 1000 * 1.05, tolerance = 0.01)
  expect_equal(multi_rate_result[[2]][1, "0.1"], 1000 * 1.10, tolerance = 0.01)
})

test_that("plot structure is correct", {
  result <- annuity_n_returns(1000, 100, 5, c(0.05, 0.10))
  # plot <- result[[1]]

  # Check that we get a plotly object
  # expect_s3_class(plot, "plotly")

  # Verify plot components
  # expect_true(!is.null(plot$x$layout$xaxis$title))
  # expect_true(!is.null(plot$x$layout$yaxis$title))

  # Check data structure
  data_table <- result[[2]]
  expect_equal(nrow(data_table), 5)  # Should have 5 years of data
  expect_equal(ncol(data_table), 3)  # 2 return rates plus year column
})

test_that("return value structure is correct", {
  result <- annuity_n_returns(1000, 100, 5, c(0.05, 0.10))

  # Check list structure
  expect_type(result, "list")
  expect_length(result, 2)

  # Check data frame components
  data_table <- result[[2]]
  expect_true("year" %in% colnames(data_table))
  expect_true(all(c("0.05", "0.1") %in% colnames(data_table)))
})

test_that("edge cases are handled properly", {
  # Test with zero initial investment
  zero_initial <- annuity_n_returns(0, 100, 5, c(0.05))
  expect_equal(nrow(zero_initial[[2]]), 5)

  # Test with zero annuity
  zero_annuity <- annuity_n_returns(1000, 0, 5, c(0.05))
  expect_equal(zero_annuity[[2]][5, "0.05"], 1000 * (1.05^5), tolerance = 0.01)

  # Test with single year
  single_year <- annuity_n_returns(1000, 100, 1, c(0.05))
  expect_equal(nrow(single_year[[2]]), 1)

  # Test with many return rates - up to 10 (cuz of the colors)
  many_rates <- annuity_n_returns(1000, 100, 5, seq(0.01, 0.10, by = 0.01))
  expect_equal(ncol(many_rates[[2]]), 11)  # 10 rates plus year column
})

test_that("compounding behavior is correct over time", {
  result <- annuity_n_returns(1000, 100, 10, c(0.05))
  data_table <- result[[2]]

  # Values should strictly increase with positive return rate and positive annuity
  expect_true(all(diff(data_table$`0.05`) > 0))

  # CLAUDE MESSED UP - Test that the growth rate increases over time (due to compounding)
  # growth_rates <- diff(data_table$`0.05`) / data_table$`0.05`[-nrow(data_table)]
  # expect_true(all(diff(growth_rates) > 0))
})

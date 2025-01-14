library(testthat)

# Test input validation
test_that("input validation works correctly", {
  # Test gross_income validation
  expect_error(taxes(-1000), "Assertion on 'gross_income' failed")
  expect_error(taxes("50000"), "Assertion on 'gross_income' failed")

  # Test state_name validation
  expect_error(taxes(50000, "NY"), "Assertion on 'state_name' failed")
  expect_error(taxes(50000, ""), "Assertion on 'state_name' failed")
})

# Test tax calculations
test_that("tax calculations are mathematically correct", {
  # Test with a simple case where we can manually calculate expected values
  result <- taxes(50000, "OK")

  # Social Security (6.2%) and Medicare (1.45%) should be exact
  soc_med <- as.numeric(strsplit(result$SocialSec_Medicare, ", ")[[1]])
  expect_equal(soc_med[1], 50000 * 0.062)
  expect_equal(soc_med[2], 50000 * 0.0145)

  # Net income should be less than gross income
  expect_lt(result$Net_Income, result$Gross_Income)

  # If_No_State_Tax should be greater than Net_Income
  expect_gt(result$If_No_State_Tax, result$Net_Income)

  # Prop_Taken should be between 0 and 1
  expect_gte(result$Prop_Taken, 0)
  expect_lte(result$Prop_Taken, 1)
})

# Test state-specific behavior
test_that("state calculations differ appropriately", {
  ok_result <- taxes(100000, "OK")
  ca_result <- taxes(100000, "CA")

  # Different states should have different tax amounts
  expect_false(ok_result$State_Taxes == ca_result$State_Taxes)

  # Different states should have different marginal rates
  expect_false(ok_result$State_Top_Marginal == ca_result$State_Top_Marginal)

  # Federal taxes should be identical
  expect_equal(ok_result$Fed_Taxes, ca_result$Fed_Taxes)

  # If_No_State_Tax should be identical
  expect_equal(ok_result$If_No_State_Tax, ca_result$If_No_State_Tax)
})

# Test edge cases
test_that("function handles edge cases correctly", {
  # Test with zero income
  zero_result <- taxes(0)
  expect_equal(zero_result$Net_Income, 0)
  expect_equal(zero_result$Fed_Taxes, 0)
  expect_equal(zero_result$State_Taxes, 0)

  # Test with very large income
  large_result <- taxes(1e7)  # $10 million
  expect_true(large_result$Prop_Taken > 0.3)  # Should have significant taxation

  # Test with income just at bracket boundaries
  # You would need to know the exact bracket boundaries from fed_tbl and state tables
  # bracket_result <- taxes(19900)  # 2021 first bracket boundary
  # expect_equal(bracket_result$Fed_Top_Marginal, "0.12, 1")
})

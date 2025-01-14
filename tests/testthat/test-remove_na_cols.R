library(testthat)
library(dplyr)

# Create test data
create_test_df <- function() {
  data.frame(
    all_na = NA_real_,
    some_na = c(1, NA, 3),
    no_na = 1:3,
    mostly_na = c(NA, NA, 1)
  )
}

test_that("input validation works", {
  test_df <- create_test_df()

  expect_error(remove_na_cols(NULL), "Assertion on 'data' failed")
  expect_error(remove_na_cols(data.frame()), "Assertion on 'data' failed")
  expect_error(remove_na_cols(test_df, threshold = -0.1), "Assertion on 'threshold' failed")
  expect_error(remove_na_cols(test_df, threshold = 1.1), "Assertion on 'threshold' failed")
  expect_error(remove_na_cols(test_df, threshold = "1"), "Assertion on 'threshold' failed")
})

test_that("default behavior removes only all-NA columns", {
  test_df <- create_test_df()
  result <- remove_na_cols(test_df)

  expect_false("all_na" %in% names(result))
  expect_true(all(c("some_na", "no_na", "mostly_na") %in% names(result)))
  expect_equal(ncol(result), 3)
})

test_that("threshold behavior works correctly", {
  test_df <- create_test_df()

  # With threshold 0.5, should remove columns with > 50% NA
  result_half <- remove_na_cols(test_df, threshold = 0.5)
  expect_false(any(c("all_na", "mostly_na") %in% names(result_half)))
  expect_true(all(c("some_na", "no_na") %in% names(result_half)))

  # With threshold 0.8, should only remove all-NA column
  result_high <- remove_na_cols(test_df, threshold = 0.8)
  expect_false("all_na" %in% names(result_high))
  expect_true(all(c("some_na", "no_na", "mostly_na") %in% names(result_high)))
})

test_that("function handles different data types", {
  test_df <- data.frame(
    na_num = NA_real_,
    na_char = NA_character_,
    na_int = NA_integer_,
    na_logical = NA,
    stringsAsFactors = FALSE
  )

  result <- remove_na_cols(test_df)
  expect_equal(ncol(result), 0)
})

test_that("function preserves data frame attributes", {
  test_df <- create_test_df()
  result <- remove_na_cols(test_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(test_df))
})

test_that("function handles edge cases", {
  # Single column data frames
  single_col_na <- data.frame(x = NA)
  single_col_data <- data.frame(x = 1)

  expect_equal(ncol(remove_na_cols(single_col_na)), 0)
  expect_equal(ncol(remove_na_cols(single_col_data)), 1)

  # Data frame with no NA values
  no_na_df <- data.frame(x = 1:3, y = 4:6)
  expect_equal(remove_na_cols(no_na_df), no_na_df)

  # All columns are NA
  all_na_df <- data.frame(x = NA, y = NA)
  expect_equal(ncol(remove_na_cols(all_na_df)), 0)
})

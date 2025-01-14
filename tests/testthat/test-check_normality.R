library(testthat)

# Helper function to create test data
create_test_data <- function(distribution = "normal") {
  set.seed(123)
  if (distribution == "normal") {
    data.frame(x = rnorm(100))
  } else if (distribution == "uniform") {
    data.frame(x = runif(100))
  } else if (distribution == "skewed") {
    data.frame(x = rexp(100))
  }
}

test_that("input validation works", {
  test_df <- create_test_data()

  expect_error(check_normality(NULL, "x"))
  expect_error(check_normality(test_df, 123))
  expect_error(check_normality(test_df, ""))
  expect_error(check_normality(test_df, "x", just_qqplot = "TRUE"))
  expect_error(check_normality(test_df, "nonexistent"))
})

test_that("Shapiro-Wilk p-value output works", {
  normal_df <- create_test_data("normal")
  skewed_df <- create_test_data("skewed")

  # Test structure of output
  normal_result <- check_normality(normal_df, "x", just_shapiro_pval = TRUE)
  # expect_s3_class(normal_result, "tbl_df")
  expect_true(all(c("method", "variable", "p.value") %in% names(normal_result)))

  # Test that p-values make sense
  normal_pval <- normal_result$p.value
  skewed_pval <- check_normality(skewed_df, "x", just_shapiro_pval = TRUE)$p.value

  expect_true(normal_pval > 0 && normal_pval < 1)
  expect_true(skewed_pval < 0.05)  # Exponential distribution should fail normality test
})

test_that("QQ plot output works", {
  test_df <- create_test_data()

  # Test that plot is created
  expect_invisible(check_normality(test_df, "x", just_qqplot = TRUE))

  # Verify plot parameters - WHAT EXACTLY IS THIS DOING?
  # plt <- check_normality(test_df, "x", just_qqplot = TRUE)
  # expect_true(!is.null(plt))
})

test_that("function handles edge cases", {
  # Single value
  single_val_df <- data.frame(x = rep(1, 10))
  expect_error(check_normality(single_val_df, "x", just_shapiro_pval = TRUE))

  # Missing values
  na_df <- data.frame(x = c(rnorm(95), rep(NA, 5)))
  expect_no_error(check_normality(na_df, "x", just_shapiro_pval = TRUE))

  # Large dataset
  large_df <- data.frame(x = rnorm(5000))
  expect_no_error(check_normality(large_df, "x", just_shapiro_pval = TRUE))
})

# CLAUDE MESSED UP ON THIS ONE I THINK
# test_that("full output includes all components", {
#   test_df <- create_test_data()
#
#   # Capture output
#   output <- capture.output({
#     result <- check_normality(test_df, "x")
#   })
#
#   # Check that Shapiro-Wilk test results are included
#   expect_true(any(grepl("Shapiro-Wilk", output)))
#
#   # Verify plots were created (both QQ plot and histogram)
#   expect_true(length(output) > 0)
# })

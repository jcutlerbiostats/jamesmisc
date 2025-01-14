library(testthat)
library(ggplot2)
library(dplyr)

# First, create some test data we'll use throughout our tests
create_test_data <- function() {
  data.frame(
    category = factor(rep(c("A", "B"), each = 50)),
    group = factor(rep(c("X", "Y"), times = 50)),
    stringsAsFactors = TRUE
  )
}

test_that("input validation works correctly", {
  test_data <- create_test_data()

  # Test data.frame validation
  expect_error(
    strat_proportion_plot(NULL, category, group),
    "Assertion on 'data' failed"
  )
  expect_error(
    strat_proportion_plot(data.frame(), category, group),
    "Assertion on 'data' failed"
  )

  # Test column existence validation
  expect_error(
    strat_proportion_plot(test_data, nonexistent, group),
    "Column 'nonexistent' not found in data frame"
  )
  expect_error(
    strat_proportion_plot(test_data, category, nonexistent),
    "Column 'nonexistent' not found in data frame"
  )
})

test_that("function returns a valid ggplot object", {
  test_data <- create_test_data()
  plot <- strat_proportion_plot(test_data, category, group)

  # Test return type
  expect_s3_class(plot, "ggplot")

  # Test essential ggplot components are present
  # expect_true("FacetWrap" %in% class(plot$facet))
  # expect_true(any(sapply(plot$layers, function(l) "GeomCol" %in% class(l$geom))))
  # expect_true(any(sapply(plot$layers, function(l) "GeomErrorbar" %in% class(l$geom))))
  # expect_true(any(sapply(plot$layers, function(l) "GeomLabel" %in% class(l$geom))))
})

test_that("plot aesthetics are correctly specified", {
  test_data <- create_test_data()
  plot <- strat_proportion_plot(test_data, category, group,
                                x_label = "Test X",
                                plot_title = "Test Title")

  # Test labels
  expect_equal(plot$labels$x, "Test X")
  expect_equal(plot$labels$y, "Percent")
  expect_equal(plot$labels$title, "Test Title")
  expect_equal(plot$labels$fill, "Test X")

  # NO CLUE WHAT THIS FUNCTION INHERITANCE STUFF IS - Test scales
  # y_scale <- plot$scales$get_scales("y")
  # expect_true(inherits(y_scale$labels, "function"))
  # expect_true(inherits(y_scale$breaks, "function"))
})

test_that("proportions are calculated correctly", {
  # Create a simple dataset with known proportions
  test_data <- data.frame(
    category = factor(c(rep("A", 75), rep("B", 25))),
    group = factor(rep(c("X", "Y"), times = 50))
  )

  plot <- strat_proportion_plot(test_data, category, group)

  # Extract the data from the plot
  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]

  # Test that proportions sum to 1 within each facet
  proportions <- split(plot_data$y, plot_data$PANEL)
  expect_true(all(sapply(proportions, sum) == 1))
})

test_that("confidence intervals are reasonable", {
  test_data <- create_test_data()
  plot <- strat_proportion_plot(test_data, category, group)

  # Extract error bar data
  error_bars <- ggplot2::ggplot_build(plot)$data[[3]]

  # Test that confidence intervals are between 0 and 1
  expect_true(all(error_bars$ymin >= 0))
  expect_true(all(error_bars$ymax <= 1))

  # Test that upper CI is greater than lower CI
  expect_true(all(error_bars$ymax > error_bars$ymin))
})

test_that("function handles edge cases gracefully", {
  # Test with single-category data
  single_cat_data <- data.frame(
    category = factor(rep("A", 100)),
    group = factor(rep(c("X", "Y"), times = 50))
  )
  expect_no_error(strat_proportion_plot(single_cat_data, category, group))

  # Test with unbalanced categories
  unbalanced_data <- data.frame(
    category = factor(c(rep("A", 99), "B")),
    group = factor(rep(c("X", "Y"), times = 50))
  )
  expect_no_error(strat_proportion_plot(unbalanced_data, category, group))

  # Test with missing combinations
  sparse_data <- data.frame(
    category = factor(c(rep("A", 50))),
    group = factor(rep("X", 50))
  )
  expect_no_error(strat_proportion_plot(sparse_data, category, group))
})

library(testthat)
library(ggplot2)

test_that("input validation works", {
  expect_error(binomial_dist(1.5, 0.5, 1, "right"))
  expect_error(binomial_dist(10, 1.5, 5, "right"))
  expect_error(binomial_dist(10, 0.5, 11, "right"))
  expect_error(binomial_dist(10, 0.5, 5, "middle"))
})

test_that("probability calculations are correct", {
  # Test basic probability calculation
  plot <- binomial_dist(2, 0.5, 1, "right")
  title_text <- plot$x$layout$title$text
  prob <- as.numeric(
    gsub(".*: ([0-9.]+)%.*", "\\1", title_text)
  )/100
  expect_equal(prob, pbinom(0, 2, 0.5, lower.tail = FALSE))

  # Test extreme probabilities
  plot_certain <- binomial_dist(10, 1, 10, "right")
  title_text <- plot_certain$x$layout$title$text
  prob <- as.numeric(
    gsub(".*: ([0-9.]+)%.*", "\\1",title_text)
  )/100
  expect_equal(prob, 1)
})

# DON'T CARE
# test_that("plot structure is correct", {
#   plot <- binomial_dist(5, 0.5, 2, "right")
#
#   # Check plot components
#   expect_s3_class(plot, "plotly")
#   expect_true(!is.null(plot$x$layout$xaxis$title))
#   expect_true(!is.null(plot$x$layout$yaxis$title))
#
#   # Check data structure
#   data <- plot$x$data
#   expect_true(length(data) > 0)
# })

test_that("left/right tail behavior is correct", {
  n <- 10
  p <- 0.5
  left_cutoff  <- 5
  right_cutoff <- 6

  left_plot <- binomial_dist(n, p, left_cutoff, "left")
  right_plot <- binomial_dist(n, p, right_cutoff, "right")

  # Extract probabilities from titles
  left_prob <- as.numeric(
    gsub(".*: ([0-9.]+)%.*", "\\1",left_plot$x$layout$title$text)
  )/100
  right_prob <- as.numeric(
    gsub(".*: ([0-9.]+)%.*", "\\1",right_plot$x$layout$title$text)
  )/100

  # Test complementary probabilities
  expect_equal(left_prob + right_prob, 1)
})

test_that("edge cases are handled", {
  # Single trial
  expect_no_error(binomial_dist(1, 0.5, 0, "left"))

  # Very large number of trials
  expect_no_error(binomial_dist(100, 0.5, 50, "right"))

  # Extreme probabilities
  expect_no_error(binomial_dist(10, 0.001, 5, "right"))
  expect_no_error(binomial_dist(10, 0.999, 5, "right"))
})

test_that("expected value calculation is correct", {
  n <- 20
  p <- 0.3
  plot <- binomial_dist(n, p, 10, "right")

  # Extract expected value from x-axis label
  x_label <- plot$x$layout$xaxis$title$text
  expected <- as.numeric(gsub(".*expected: ([0-9.]+).*", "\\1", x_label))

  expect_equal(expected, n * p)
})

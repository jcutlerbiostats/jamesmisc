#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_y_continuous labs facet_wrap geom_errorbar geom_label geom_col scale_fill_manual
#' @importFrom scales percent pretty_breaks
NULL

#' Plot bivariate proportions with confidence intervals
#'
#' This function takes a data frame, a categorical row variable, and a categorical column variable, and
#' plots the proportions of the row variable categories stratified by (faceted by) the column variable categories.
#' Error bars represent the 95% confidence intervals for the proportions.
#'
#' @param data A data frame.
#' @param row_var The categorical row variable (not passed as a string).
#' @param col_var The categorical column variable (not passed as a string). The counts and percents of the row variable will be stratified by the column variable.
#' @param x_label A character. The X axis label.
#' @param plot_title A character. The plot title.
#'
#' @return A ggplot of the stratified proportions with CI errorbars
#'
#' @examples
#' # Plot of counts and % of cylinder categories stratified by transmission category in mtcars
#' strat_proportion_plot(mtcars,cyl,am,"Cylinder","Number of cylinders (count, %) by transmission type")
#'
#' @export
strat_proportion_plot <- function(data,
                                  row_var,
                                  col_var,
                                  x_label = "Row Variable", # Can be NULL too
                                  plot_title = "Stratified Counts and Percentages"){ # plot_title can be NULL too
  checkmate::assert_data_frame(data,min.rows = 1,min.cols = 1)
  # Check if the row_var exists
  tryCatch(
    {
      rlang::eval_tidy(rlang::quo(dplyr::pull(data, {{row_var}})))
      invisible(TRUE)
    },
    error = function(e) {
      stop("Column '", rlang::as_label(rlang::enquo(row_var)), "' not found in data frame")
    }
  )
  # Check if the col_var exists
  tryCatch(
    {
      rlang::eval_tidy(rlang::quo(dplyr::pull(data, {{col_var}})))
      invisible(TRUE)
    },
    error = function(e) {
      stop("Column '", rlang::as_label(rlang::enquo(col_var)), "' not found in data frame")
    }
  )

  data %>%
    # Just put the column variable first, row variable second, here. freqtable confusingly calls
    # them the opposite though, and then stratifies the column variable by the row variable.
    freqtables::freq_table({{col_var}},{{row_var}}) %>%

    dplyr::select(1:8,13,14,16:17) %>%
    dplyr::mutate(dplyr::across(c(percent_row,lcl_row,ucl_row),~./100)) %>%
    dplyr::mutate(row_cat = paste(row_var,row_cat,sep="=")) %>%

    ggplot2::ggplot(ggplot2::aes(col_cat,percent_row,fill=col_cat)) +
    # freqtable's row variable is actually being used as the column variable, confusingly enough. Oh well.
    ggplot2::facet_wrap(~row_cat) +
    ggplot2::geom_col() +
    ggplot2::geom_label(ggplot2::aes(label = paste(round(percent_row*100,1),"%","\nn/N = ",n,"/",n_row)),
                        size = 3,fill = "white") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lcl_row,ymax = ucl_row),
                           width = .25,linetype = "dashed",alpha = .5) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                labels = scales::percent) +
    ggplot2::labs(
      title = plot_title,
      x     = x_label,
      fill  = x_label,
      y     = "Percent"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = c("#2c3e50", "#e31a1c", "#18BC9C", "#e9d8a6", "#ade8f4", "#3498DB", "#7570b3", "#a1d99b","#ef476f","#bb9457"))
}

#' @importFrom magrittr %>%
#' @importFrom ggplot2 scale_x_continuous
NULL

#' Dot-whisker plot of odds ratios and 95% confidence intervals from a multivariable logistic regression model
#'
#' This function takes a glm object (the multivariable logistic regression model) and returns a ggplot of the
#' estimated adjusted odds ratios for each covariate, and corresponding 95% confidence intervals.
#'
#' @param model A fitted `glm` model.
#' @param title A character, by default set to 'Adjusted Odds Ratios with 95% CI'.
#' @param subtitle A character. Default is NULL.
#' @param log_scale A logical. Would you like to see the odds ratios on a log scale? TRUE/FALSE. Default is TRUE.
#'
#' @return A dot-whisker plot of the estimated adjusted odds ratios with 95% confidence intervals.
#'
#' @examples
#' # Visualize the relative odds of not surviving the Titanic
#' # E.g., For males vs. females, after adjusting for class and age group
#' library(dplyr)
#'
#' data("Titanic")
#'
#' titanic_tbl <- Titanic %>%
#' tibble::as_tibble() %>%
#' tidyr::uncount(n) %>%
#' mutate(
#'   Survived = factor(Survived,levels = c("Yes","No")) %>% as.numeric(),
#'   Survived = Survived - 1,
#'   Survived = factor(Survived)
#'  )
#'
#' titanic_fit <- glm(Survived ~ Sex + Age + Class,data = titanic_tbl,family = binomial)
#'
#' plot_logistic_results(
#' titanic_fit,
#' subtitle = "Relative odds of not surviving the Titanic"
#' )
#'
#' @export
plot_logistic_results <- function(model,
                                  title = "Adjusted Odds Ratios with 95% CI",
                                  subtitle = NULL,
                                  log_scale = TRUE) {
  checkmate::assert_class(model,"glm")
  checkmate::assert(
    checkmate::check_true(
      model$family$family == "binomial" && model$family$link == "logit"
    ),
    msg = "Model must be a logistic regression (family = binomial) with logit link"
  )
  checkmate::assert_character(title)
  checkmate::assert_character(subtitle,null.ok = TRUE)
  checkmate::assert_flag(log_scale)

  # Get tidy results with confidence intervals, exponentiated for odds ratios
  mod_tidy <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    # Optionally remove intercept
    dplyr::filter(term != "(Intercept)") %>%
    # Clean up variable names
    dplyr::mutate(
      term = stringr::str_replace_all(term, "_", " ") %>%
        stringr::str_to_title(),
      # Add significance markers
      sig_label = dplyr::case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    )

  g <- mod_tidy %>%
    # Create the plot
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = term)) +
    # Reference line at OR = 1
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",linewidth = 1.2, color = "gray40",alpha = .6) +
    # Point estimates
    ggplot2::geom_point(size = 3) +
    # Confidence intervals
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low, xmax = conf.high),
                            height = 0.2) +
    # Significance markers
    ggplot2::geom_text(ggplot2::aes(label = sig_label, x = conf.high),
                       hjust = -0.5, size = 4) +
    # Use log scale for better visualization of ratios
    ggplot2::scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) round(10^x, 2)),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(size = 10),
      plot.caption = ggplot2::element_text(color = "gray20", face = "italic")
    )

  if(isFALSE(log_scale)){
    print(
      g +
        ggplot2::scale_x_continuous() +
        ggplot2::labs(
          title = title,
          subtitle = subtitle,
          x = "Adjusted Odds Ratio",
          y = "Covariate",
          caption = "Whiskers represent 95% confidence intervals\nSignificance: *** p<0.001, ** p<0.01, * p<0.05, ns p≥0.05"
        )
    )
  }
  else{
    print(
      g +
        ggplot2::labs(
          title = title,
          subtitle = subtitle,
          x = "Adjusted Odds Ratio (log scale)",
          y = "Covariate",
          caption = "Whiskers represent 95% confidence intervals\nSignificance: *** p<0.001, ** p<0.01, * p<0.05, ns p≥0.05"
        )
    )
  }
}

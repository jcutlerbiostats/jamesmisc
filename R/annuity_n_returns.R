#' @importFrom magrittr %>%
#' @importFrom scales dollar pretty_breaks
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous scale_x_continuous element_text element_blank labs scale_color_manual
#' @importFrom plotly ggplotly
NULL

#' Compute investment growth with annuities for a variety of annual return rates
#'
#' This function takes an initial investment amount, an amount for annuities
#' added to the investment every year, a vector of annual return rates, and years of investment,
#' and returns an interactive plot displaying the performance of the various return rates
#' over the specified period, as well as a tibble with the data displayed in the plot.
#'
#' @param initial_investment A numeric representing the initial investment amount.
#' @param annuity A numeric representing the amount added to the investment every year.
#' @param years A numeric representing the number of years of the investment period.
#' @param return_rates A numeric vector representing the specified annual return rates.
#'
#' @return A list with: 1) a plotly plot object displaying the growth curves of the various investments, and 2) a tibble containing the data displayed in the plot.
#'
#' @examples
#' # How do 6%, 7%, 8%, 9%, and 10% annual return rates compare after ...
#' # ... 25 years of an initial $20,000 investment, & $10,000 annual contributions?
#' list_data_and_plot <- annuity_n_returns(
#'  20000,
#'  10000,
#'  25,
#'  c(.06,.07,.08,.09,.1)
#' )
#'
#' list_data_and_plot[[1]]
#'
#' @export
annuity_n_returns <- function(initial_investment,
                              annuity,
                              years,
                              return_rates=c(.06,.07,.08,.09,.1)){
  checkmate::assert_numeric(initial_investment,lower = 0,any.missing = FALSE)
  checkmate::assert_numeric(annuity,lower = 0,any.missing = FALSE)
  checkmate::assert_numeric(years,lower = 1,any.missing = FALSE)
  checkmate::assert_numeric(return_rates,any.missing = FALSE)

  terminalAnnuity <- vector(mode = "list", length = length(return_rates))
  resetInvestment <- initial_investment
  investment      <- initial_investment
  for (j in 1:length(return_rates)){
    addYearly  = annuity       # yearly's value needs to be preserved after the next line of code where I change it to zero.
    annuity    = 0
    investment = resetInvestment
    for (i in 1:years){
      terminalAnnuity[[j]][i] = (investment + annuity)*(1 + return_rates[j])
      annuity    = addYearly   # this is why it needs to be preserved
      investment = terminalAnnuity[[j]][i]
    }
  }
  terminalAnnuity <- do.call("cbind",terminalAnnuity)
  terminalAnnuity <- as.data.frame(terminalAnnuity)

  colnames(terminalAnnuity) <- return_rates
  terminalAnnuity           <- terminalAnnuity %>% dplyr::mutate(year = dplyr::row_number())
  annuity_long_tbl          <- terminalAnnuity %>% tidyr::pivot_longer(!year)

  g <- ggplot2::ggplot(
    annuity_long_tbl,
    ggplot2::aes(x = year,
                 y = value,
                 color = name,
                 group = name,
                 text = paste("Year: ",year,"\n",
                              "Amount: ",scales::dollar(value),"\n",
                              "Return Rate: ",name))
  ) +

    ggplot2::geom_line(size = 1.1) +

    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n=8),
                                labels = scales::dollar) +
    ggplot2::scale_x_continuous(breaks = annuity_long_tbl$year) +

    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values = c("#2c3e50", "#e31a1c", "#18BC9C", "#e9d8a6", "#ade8f4", "#3498DB", "#7570b3", "#a1d99b","#ef476f","#bb9457")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30,vjust = .9,hjust = .9),
                   panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::labs(
      title    = glue::glue("Initial investment of {dollar(initial_investment)}, then annuities of {dollar(annuity)}"),
      color    = "Annual Return Rate",
      x = "Years out from initial investment",
      y = "Investment Growth"
    )

  p <- plotly::ggplotly(g,tooltip = "text")

  mylist = list(p,terminalAnnuity)

  return(mylist)
}

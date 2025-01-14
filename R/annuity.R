#' Compute investment growth with annuities
#'
#' This function takes an initial investment amount, an amount for annuities
#' added to the investment every year, an annual return rate, and years of investment,
#' and returns a numeric vector representing the new amount every year.
#'
#' @param return_rate A numeric representing the annual return rate.
#' @param initial_investment A numeric representing the initial investment amount.
#' @param annuity A numeric representing the amount added to the investment every year.
#' @param years A numeric representing the number of years of the investment period.
#'
#' @return A numeric vector representing the size of the investment each year.
#'
#' @examples
#' # With an initial investment of $10,000 and yearly contributions of $5,000 ...
#' # ... what will the size of the investment be after 25 years of 8% annual returns?
#' annuity(.08,10000,5000,25)
#'
#' @export
annuity <- function(return_rate,
                    initial_investment,
                    annuity,
                    years){
  checkmate::assert_numeric(return_rate,any.missing = FALSE)
  checkmate::assert_numeric(initial_investment,lower = 0,any.missing = FALSE)
  checkmate::assert_numeric(annuity,lower = 0,any.missing = FALSE)
  checkmate::assert_numeric(years,lower = 1,any.missing = FALSE)

  addAnnualInvestments <- annuity
  annuity <- 0

  terminal <- vector()
  for(i in 1:years){
    terminal[i] <- (initial_investment + annuity)*(1 + return_rate)
    annuity     <- addAnnualInvestments
    initial_investment <- terminal[i]
  }
  return(terminal)
}

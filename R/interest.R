#' Compute exponential growth or decay / compute compound interest
#'
#' This function takes an annual return rate, an initial investment amount, and
#' number of years of accrued interest, and returns the terminal amount at the end
#' of the specified period.
#'
#' @param return_rate A numeric representing the annual return rate. A negative value would indicate a decay rate.
#' @param initial_investment A numeric representing the initial amount invested.
#' @param years A numeric representing the number of years during which the initial investment grows at the specified return rate.
#'
#' @return A numeric representing the terminal amount, after accruing the specified interest over the specified period.
#'
#' @examples
#' # How much will you have after 30 years of an intial $10,000 growing at 7% per year?
#' interest(.07,10000,30)
#'
#' @export
interest <- function(return_rate,
                     initial_investment,
                     years){
  checkmate::assert_numeric(return_rate)
  checkmate::assert_numeric(initial_investment,lower = 0)
  checkmate::assert_numeric(years,lower = 1)

  terminal <- initial_investment*(1 + return_rate)^years
  return(terminal)
}

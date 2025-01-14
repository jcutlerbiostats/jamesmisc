#' @importFrom magrittr %>%
NULL

#' Compute your net income from your gross income, after factoring in federal and state taxes
#'
#' This function takes a numeric value representing your gross income and returns a tibble
#' with information like net income, the amount you pay in federal taxes and state taxes (possible
#' states are Oklahoma and California), and other things. Married filing jointly is assumed.
#'
#' @param gross_income A numeric positive value representing your gross annual income.
#' @param state_name A character. 'OK' for Oklahoma, and 'CA' for California. Default is 'OK'.
#'
#' @return A tibble with the following things: Your federal top marginal rate, state top marginal rate, federal taxes, state taxes, amount paid to social security and medicare, proportion removed from gross income, net income, and what your net income would be if there were no state taxes.
#'
#' @examples
#' # What is your net income in Oklahoma if your gross income is $80,000?
#' taxes(80000)
#'
#' @export
taxes <- function(gross_income,
                  state_name = "OK"){
  checkmate::assert_numeric(gross_income,lower = 0)
  checkmate::assert_choice(state_name,choices = c("OK","CA"))

  # Federal taxes
  fed_tbl %>%
    dplyr::filter(joint_lb <= gross_income) %>%
    dplyr::select(c(dplyr::matches("joint"),rate)) %>%

    dplyr::rowwise() %>%
    dplyr::mutate(brackets = joint_ub - joint_lb) %>%
    dplyr::ungroup() %>%

    dplyr::mutate(brackets = dplyr::case_when(
      rate == max(rate) ~ gross_income - joint_lb,
      TRUE ~ brackets
    )) %>%

    dplyr::rowwise() %>%
    dplyr::mutate(tax = rate*brackets) %>%
    dplyr::ungroup() %>%
    dplyr::pull(tax) %>% sum() -> fed_taxed

  # Social Security and Medicare
  social_sec <- gross_income * .062
  medicare <- gross_income * .0145

  # State taxes
  if(state_name == "OK"){
    okla_tbl %>%
      dplyr::filter(joint_lb <= gross_income) %>%
      dplyr::select(c(dplyr::matches("joint"),rate)) %>%

      dplyr::rowwise() %>%
      dplyr::mutate(brackets = joint_ub - joint_lb) %>%
      dplyr::ungroup() %>%

      dplyr::mutate(brackets = dplyr::case_when(
        rate == max(rate) ~ gross_income - joint_lb,
        TRUE ~ brackets
      )) %>%

      dplyr::rowwise() %>%
      dplyr::mutate(tax = rate*brackets) %>%
      dplyr::ungroup() %>%
      dplyr::pull(tax) %>% sum() -> state_taxed

    ## State Top Marginal Rate
    state_rate <- okla_tbl %>%
      dplyr::filter(joint_lb <= gross_income) %>%
      dplyr::select(rate,joint_lb) %>%
      tail(1) %>%
      dplyr::mutate(prop = (gross_income - joint_lb) / gross_income,
                    prop = round(prop,3)) %>%
      tidyr::unite("Rate_and_Proportion_Taxed",rate:prop,sep = ", ") %>%
      dplyr::pull(Rate_and_Proportion_Taxed)
  }
  if(state_name == "CA"){
    cali_tbl %>%
      dplyr::filter(joint_lb <= gross_income) %>%
      dplyr::select(c(dplyr::matches("joint"),rate)) %>%

      dplyr::rowwise() %>%
      dplyr::mutate(brackets = joint_ub - joint_lb) %>%
      dplyr::ungroup() %>%

      dplyr::mutate(brackets = dplyr::case_when(
        rate == max(rate) ~ gross_income - joint_lb,
        TRUE ~ brackets
      )) %>%

      dplyr::rowwise() %>%
      dplyr::mutate(tax = rate*brackets) %>%
      dplyr::ungroup() %>%
      dplyr::pull(tax) %>% sum() -> state_taxed

    ## State Top Marginal Rate
    state_rate <- cali_tbl %>%
      dplyr::filter(joint_lb <= gross_income) %>%
      dplyr::select(rate,joint_lb) %>%
      tail(1) %>%
      dplyr::mutate(prop = (gross_income - joint_lb) / gross_income,
                    prop = round(prop,3)) %>%
      tidyr::unite("Rate_and_Proportion_Taxed",rate:prop,sep = ", ") %>%
      dplyr::pull(Rate_and_Proportion_Taxed)
  }

  # Subtract
  gross_income - (fed_taxed + social_sec + medicare + state_taxed) -> your_net

  # Other output tibble ingredients
  ## Fed Top Marginal Rate
  fed_rate <- fed_tbl %>%
    dplyr::filter(joint_lb <= gross_income) %>%
    dplyr::select(rate,joint_lb) %>%
    tail(1) %>%
    dplyr::mutate(prop = (gross_income - joint_lb) / gross_income,
                  prop = round(prop,3)) %>%
    tidyr::unite("Rate_and_Proportion_Taxed",rate:prop,sep = ", ") %>%
    dplyr::pull(Rate_and_Proportion_Taxed)

  # [STATE TOP MARGINAL RATE - SEE IF LOGIC ABOVE]

  ## IF YOU HAD NO STATE TAXES
  zero_state_net <- gross_income - (fed_taxed + social_sec + medicare)

  # Output tibble
  output_tbl <- tibble::tibble(
    Gross_Income = gross_income,
    Tax_Filing   = "Married",
    Fed_Top_Marginal   = fed_rate,
    State_Top_Marginal = state_rate,
    # State         = state_name,
    Fed_Taxes       = fed_taxed,
    State_Taxes     = state_taxed,
    SocialSec_Medicare = paste(round(social_sec,0),round(medicare,0),sep = ", "),
    # Medicare      = medicare,
    Prop_Taken      = (gross_income - your_net) / gross_income,
    Net_Income      = your_net,
    If_No_State_Tax = zero_state_net
  )

  return(output_tbl)
}

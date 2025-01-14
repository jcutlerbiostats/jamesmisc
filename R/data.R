#' Federal Tax Brackets
#'
#' This dataset contains the federal tax rate for each tax bracket.
#'
#' @format A data frame with 7 rows and 7 columns:
#' \describe{
#'   \item{rate}{The marginal tax rate}
#'   \item{single_lb}{The tax bracket lower bound for filing single that goes with the corresponding marginal tax rate}
#'   \item{single_ub}{The tax bracket upper bound for filing single that goes with the corresponding marginal tax rate}
#'   \item{joint_lb}{The tax bracket lower bound for married filing jointly that goes with the corresponding marginal tax rate}
#'   \item{joint_ub}{The tax bracket upper bound for married filing jointly that goes with the corresponding marginal tax rate}
#'   \item{head_lb}{The tax bracket lower bound for filing head of household that goes with the corresponding marginal tax rate}
#'   \item{head_ub}{The tax bracket upper bound for filing head of household that goes with the corresponding marginal tax rate}
#' }
#'
"fed_tbl"

#' Oklahoma State Tax Brackets
#'
#' This dataset contains the state tax rate of Oklahoma for each tax bracket.
#'
#' @format A data frame with 7 rows and 5 columns:
#' \describe{
#'   \item{rate}{The marginal tax rate}
#'   \item{single_lb}{The tax bracket lower bound for filing single that goes with the corresponding marginal tax rate}
#'   \item{single_ub}{The tax bracket upper bound for filing single that goes with the corresponding marginal tax rate}
#'   \item{joint_lb}{The tax bracket lower bound for married filing jointly that goes with the corresponding marginal tax rate}
#'   \item{joint_ub}{The tax bracket upper bound for married filing jointly that goes with the corresponding marginal tax rate}
#' }
#'
"okla_tbl"

#' California State Tax Brackets
#'
#' This dataset contains the state tax rate of California for each tax bracket.
#'
#' @format A data frame with 9 rows and 3 columns:
#' \describe{
#'   \item{rate}{The marginal tax rate}
#'   \item{joint_lb}{The tax bracket lower bound for married filing jointly that goes with the corresponding marginal tax rate}
#'   \item{joint_ub}{The tax bracket upper bound for married filing jointly that goes with the corresponding marginal tax rate}
#' }
#'
"cali_tbl"

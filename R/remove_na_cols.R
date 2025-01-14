#' @importFrom magrittr %>%
NULL

#' Remove columns in a data frame that only have NA values (or that exceed a specified proportion of NA values)
#'
#' This function takes a data frame and automatically removes the columns
#' that only contain NA values. Also, the user can set a desired threshold for removing
#' columns with a proportion of NA values exceeding that threshold.
#'
#' @param data A data frame or tibble. If there are NA-only columns, they will be removed, though it isn't necessary for data to have NA-only columns.
#' @param threshold A numeric between 0 and 1. Default is 1 (i.e., 100%). Represents the threshold proportion of NA values if the user wants to remove columns that are not purely NA, but whose proportion of NA values exceeds the specified threshold.
#'
#' @return Your data frame, minus the columns that were removed
#'
#' @examples
#' # Remove columns that are NA-only
#' library(dplyr)
#'
#' mtcars$drat <- NA_real_
#' mtcars_new <- mtcars %>% remove_na_cols()
#' mtcars_new
#'
#' # Set a threshold proportion of NA values for column-removal
#' library(dplyr)
#'
#' mtcars$drat <- NA_real_
#' mtcars %>%
#'   mutate(drat = case_when(mpg > 31 ~ 3.5)) %>%
#'   remove_na_cols(threshold = .93) # Change threshold to .94 and drat will remain
#'
#' @export
remove_na_cols <- function(data,
                           threshold=1){
  checkmate::assert_data_frame(data,min.rows = 1,min.cols = 1)
  checkmate::assert_numeric(threshold,lower = 0,upper = 1)

  data %>%
    # Compute proportion of values that are NA
    dplyr::summarise_all(~sum(is.na(.))/length(.)) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    # Keep only column names with NA proportions under the threshold
    dplyr::filter(value < threshold) %>% dplyr::pull(name) -> cols_you_want

  data %>% dplyr::select(cols_you_want)
}

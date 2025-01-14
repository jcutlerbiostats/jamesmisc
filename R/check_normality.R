#' @importFrom magrittr %>%
NULL

#' Asses normality using a QQ plot, a histogram with normal density curve superimposed, and a Shapiro-Wilk test for normality
#'
#' This function takes a data frame and a string of the variable name for
#' which you wish to assess normality, and outputs the QQ plot, the histogram with
#' superimposed normal curve, and the results of the Shapiro-Wilk test.
#'
#' @param data A data frame with the variable of interest.
#' @param variable_name A character; the name of the variable of interest. The variable's sample size must be between 3 and 5,000, inclusive.
#' @param just_qqplot A logical; do you want just the qqplot? TRUE/FALSE. Default is FALSE.
#' @param just_shapiro_pval A logical; do you want just the Shapiro-Wilk p-value? TRUE/FALSE. Default is FALSE.
#'
#' @return A qqplot, a histogram of the variable's distribution with a normal curve superimposed, and the results of a Shapiro-Wilk test.
#'
#' @examples
#' # Assess the normality of mpg in mtcars
#' check_normality(ChickWeight,"weight")
#'
#' @export
check_normality <- function(data,
                            variable_name,
                            just_qqplot = F,
                            just_shapiro_pval = F){
  checkmate::assert_data_frame(data,min.rows = 1,min.cols = 1)
  checkmate::assert_character(variable_name,min.chars = 1)
  checkmate::assert_flag(just_qqplot)
  checkmate::assert_flag(just_shapiro_pval)
  checkmate::assert(
    checkmate::check_true(!(just_qqplot && just_shapiro_pval)),
    msg = "flags cannot both be TRUE"
  )

  myvar <- data %>% dplyr::pull(variable_name)

  checkmate::assert_numeric(myvar,min.len = 3,max.len = 5000)

  if(isTRUE(just_qqplot)){
    invisible(myvar %>% car::qqPlot(main = variable_name))
  }
  if(isTRUE(just_shapiro_pval)){
    shapiro_df <- shapiro.test(myvar) %>%
      broom::tidy() %>%
      dplyr::mutate(variable = variable_name) %>%
      dplyr::select(method,variable,p.value)
    return(shapiro_df)
  }
  if(isFALSE(just_qqplot) & isFALSE(just_shapiro_pval)){
    myvar %>% car::qqPlot(main = variable_name)

    myvar %>% hist(prob=T,main = variable_name)
    lines(density(na.omit(myvar)),col="red",lwd=2)
    xfit <- seq(min(myvar,na.rm = T),max(myvar,na.rm = T),length=100)
    yfit <- dnorm(xfit,mean=mean(myvar,na.rm = T),sd=sd(myvar,na.rm = T))
    lines(xfit,yfit,col="blue",lwd=2,lty=2)

    shapiro.test(myvar)
  }
}

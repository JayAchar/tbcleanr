#' Xpert variable detector
#'
#' Find Xpert variables within data frame by using Regex. Adjust
#' when lab changes variable names
#' @param x data frame with xpert variables
#' @importFrom stringr str_detect
#' @return string of variable names matching regex for Xpert results

xpert_variable_detector <- function(x) {
  . <- NULL
  # new xpert variable names added in late 2018
  xpert_variable_regex <- c("^g[en]*x_res\\d$")
  
  xpert_additions <- names(x) %>%
    tolower() %>%
    str_detect(pattern = xpert_variable_regex) %>% 
    subset(names(x), subset = .)
  
  xpert_additions
}

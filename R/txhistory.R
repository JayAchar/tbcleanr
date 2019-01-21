#' Clean treatment history variable
#'
#' @param x data frame of TB admission data
#'
#' @return data frame with factorised treatment history
#' variable
#' @importFrom assertthat assert_that
#' @export

txhistory <- function(x) {
  assert_that(is.data.frame(x))
  
  UseMethod("txhistory", x)
}
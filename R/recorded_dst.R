#' Clean recorded baseline DST variable
#'
#' @param x data frame of TB admission data
#'
#' @return data frame with factorised baseline DST
#' @importFrom assertthat assert_that
#' @export

recorded_dst <- function(x) {
  assert_that(is.data.frame(x))
  
  UseMethod("recorded_dst", x)
}


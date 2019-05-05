#' Clean drug adherence data
#'
#' Take drug adherence data from routine TB programmes and clean for 
#' further analysis.
#' 
#' @param x data frame containing drug adherence data
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

adhere_cleanr <- function(x) {
  
  # check input
  assert_that(is.data.frame(x))
  
  # apply useMethod approach
  UseMethod("adhere_cleanr", x)
}

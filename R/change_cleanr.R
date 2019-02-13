#' Clean drug change data
#'
#' Take drug change data from routine TB programmes and clean for 
#' further analysis.
#' 
#' @param x data frame containing drug variables
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

change_cleanr <- function(x) {
  
  # check input
  assert_that(is.data.frame(x))
  
  # apply useMethod approach
  UseMethod("change_cleanr", x)
}

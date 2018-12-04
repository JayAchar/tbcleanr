#' Consolidate smear and culture results
#'
#' Take laboratory data set and consolidate repeated results to give summary variable
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertthat assert_that

result_consolidator <- function(x, ...) {
  
  # check input
  assert_that(is.data.frame(x))
  
  UseMethod("result_consolidator", x)
}
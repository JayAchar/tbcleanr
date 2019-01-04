#' Default method for result_consolidator()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export 

result_consolidator.default <- function(x, ...) {
  
  message("No adm object class detected: result_consolidator() not applied.")
  x       
}

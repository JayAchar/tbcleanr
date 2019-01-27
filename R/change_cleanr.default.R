#' Default method for change_cleanr()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export 

change_cleanr.default <- function(x) {
  
  message("No change object class detected: change_cleanr() not applied.")
  x       
}

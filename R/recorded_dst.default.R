#' Default method for recorded_dst()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export 

recorded_dst.default <- function(x) {
  
  message("No adm object class detected: recorded_dst() not applied.")
  x       
}

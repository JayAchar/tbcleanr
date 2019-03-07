#' Default method for txhistory()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export 

txhistory.default <- function(x) {
  
  # check whether adm object class can be applied
  y <- adm_classr(x)
  
  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {
    
    adm_subset(y)
    
  } else {
  message("No adm object class detected: txhistory() not applied.")
  x          
  
  }
 
}

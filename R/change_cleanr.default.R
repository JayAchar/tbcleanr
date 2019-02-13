#' Default method for change_cleanr()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @export 

change_cleanr.default <- function(x) {
  
  
  k6_names <- c("RegistrationNb", "changedate", "SEdescrip", "Datepi")
  epiinfo_names <- c("APID", "DACHAN", "STARTTRE", "MEDTT")
  
  if (all(k6_names %in% names(x))) {
    
    class(x) <- c(class(x), "koch6")
    change_cleanr(x)
    
  } else if (all(epiinfo_names %in% names(x))) {
    class(x) <- c(class(x), "epiinfo")
    change_cleanr(x)
    
  } else {
    
    message("No change object class detected: change_cleanr() not applied.")  
    x  
  }
  
}

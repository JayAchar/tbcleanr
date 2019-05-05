#' Default method for adhere_cleanr()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @export 

adhere_cleanr.default <- function(x) {

  # check whether adm object class can be applied
  y <- adhere_classr(x)
  
  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {
    
    adhere_cleanr(y, adm = adm)
    
  } else {
    
    message("No adherence object class detected: adhere_cleanr() not applied.")
    x
  }
  
  
}

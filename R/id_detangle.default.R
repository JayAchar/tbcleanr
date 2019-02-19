#' Default method for id_detangle()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing admission ID variable
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @export 

id_detangle.default <- function(x, ...) {
  
  # check whether adm object class can be applied
  y <- adm_classr(x)
  
  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {
    
    id_detangle(y)
    
  } else {
    
    message("No adm object class detected: id_detangle() not applied.")
    x       

  }
    
}

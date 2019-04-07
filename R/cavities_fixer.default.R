#' Convert admission x-ray variables to factorised output
#'
#' @param x data frame containing admission x-ray variables
#' @param ... further arguments passed to or from other methods
#'
#' @return admission data frame with factor x-ray summary varialbe
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export
#'

cavities_fixer.default <- function(x, ...) {
  
  # check whether adm object class can be applied
  y <- adm_classr(x)
  
  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {
    
    cavities_fixer(y)
    
  } else {
    
    message("No adm object class detected: cavities_fixer() not applied.")
    x  
    
  }
  
  
}

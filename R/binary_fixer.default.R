#' Default method for binary_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}

binary_fixer.default <- function(x, ...) {
    
    message("No adm object class detected: adm_subset() not applied.")
    x  
  
}
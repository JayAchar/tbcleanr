#' Default method for id_detangle()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing admission ID variable
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export 

id_detangle.default <- function(x, ...) {
    
    message("No adm object class detected: id_detangle() not applied.")
    x       
}

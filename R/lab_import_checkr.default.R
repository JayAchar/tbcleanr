#' Default method for lab_import_checkr()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}

lab_import_checkr.default <- function(x, ...) {
  
  message("No lab object class detected: lab_import_checkr() not applied.")
  x       
}

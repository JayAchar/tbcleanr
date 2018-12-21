#' Default method for lab_date_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing admission ID variable
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}

lab_date_fixer.default <- function(x, ...) {
  
  message("No lab object class detected: lab_date_fixer() not applied.")
  x       
}

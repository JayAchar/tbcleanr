#' Default method for gender_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export 

gender_fixer.default <- function(x, ...) {

    message("No adm object class detected: gender_fixer() not applied.")
    x       
}

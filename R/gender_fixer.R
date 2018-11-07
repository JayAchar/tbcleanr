#' Factorise gender variable
#'
#' Take gender variable from data frame and factorise. Requires that variable names
#' have been pre-adjusted.  
#' @param x data frame containing Koch 6 admission variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertthat assert_that

gender_fixer <- function(x, ...) {

# check input
    assert_that(is.data.frame(x))
    
# UseMethod
    UseMethod("gender_fixer", x)
}
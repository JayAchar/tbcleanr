#' Split admission ID number
#'
#' Take admission ID number, check that it is unique and generate additional information 
#' @param x data frame containing admission ID variable
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertthat assert_that

id_detangle <- function(x, ...) {
    
    # check input
    assert_that(is.data.frame(x))
    
    # UseMethod
    UseMethod("id_detangle", x)
}

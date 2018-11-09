#' Fix TB treatment outcomes
#'
#' Combine treatment outcome variables to leave one 
#' factorised, labelled variable
#' @param x data frame containing outcome variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export

outcome_fixer <- function(x, ...) {

    # check input
    assert_that(is.data.frame(x))
    
    # apply correct method
    UseMethod("outcome_fixer", x)

}


#' Convert admission binary variables to factorised output
#'
#' Take admission data frame with binary variables and output 
#' binary factorised variables 
#' @param x data frame containing admission binary variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export


binary_fixer <- function(x, ...) {

# check input
    assert_that(is.data.frame(x))

# useMethod
    UseMethod("binary_fixer", x)
}

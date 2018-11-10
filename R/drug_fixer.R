#' Convert drug doses to binary output
#'
#' Take data frame with drug dosing variables and output 
#' binary factorised variables 
#' @param x data frame containing drug variables
#' @param ... further arguments passed to or from other methods.
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

drug_fixer <- function(x, ...) {
# check input
    assert_that(is.data.frame(x))

# apply useMethod approach
    UseMethod("drug_fixer", x)
}

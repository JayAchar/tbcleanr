#' Factorise HIV variable
#'
#' Take HIV variables from data frame and factorise. Use software
#' to define which data base is being used as the input
#' @param x data frame containing Koch 6 admission variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

hiv_fixer <- function(x, ...) {

# check input
  assert_that(is.data.frame(x))

# call correct method
    UseMethod("hiv_fixer", x)
x
}

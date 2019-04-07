#' Subset adm variables
#'
#' Subset pre-specified TB admission variables
#' @param x data frame containing variables
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

adm_subset <- function(x, add = NULL, ...) {

# check arg
    assert_that(is.character(add) | is.null(add))
    assert_that(is.data.frame(x))

    UseMethod("adm_subset", x)
}
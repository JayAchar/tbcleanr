#' Subset lab variables
#'
#' Subset pre-specified TB laboratory variables
#' @param x data frame containing variables
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export


lab_subset <- function(x, add = NULL, ...) {
  
# check input
  assert_that(is.character(add) | is.null(add))
	assert_that(is.data.frame(x))

# apply useMethod
  UseMethod("lab_subset", x)

}


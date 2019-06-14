#' Consolidate Xpert results
#'
#' Take laboratory data set and consolidate Xpert results
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

xpert_result_fixer <- function(x, ...) {

  # check input
  assert_that(is.data.frame(x))

  # UseMethod
  UseMethod("xpert_result_fixer", x)

}


#' Default method for xpert_result_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing admission ID variable
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export

xpert_result_fixer.default <- function(x, ...) {

  message("No lab object class detected: xpert_result_fixer() not applied.")
  x
}

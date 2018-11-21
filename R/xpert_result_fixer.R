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

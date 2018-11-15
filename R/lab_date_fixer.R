#' Aggregate dates in lab database
#'
#' Consolidate sample dates in mycobacterial lab database
#' @param x data frame containing sample date variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertthat assert_that

lab_date_fixer <- function(x, ...) {
# check input
  assert_that(is.data.frame(x))

  UseMethod("lab_date_fixer", x)

}

#' Aggregate dates in lab database
#'
#' Consolidate sample dates in mycobacterial lab database
#' @param x data frame containing sample date variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertthat assert_that

lab_date_fixer <- function(x, ...) {
# check input
  assert_that(is.data.frame(x))

  UseMethod("lab_date_fixer", x)

}


#' Default method for lab_date_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @inheritParams lab_date_fixer
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @export

lab_date_fixer.default <- function(x, ...) {

  message("No lab object class detected: lab_date_fixer() not applied.")
  x
}

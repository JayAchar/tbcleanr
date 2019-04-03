#' Convert admission x-ray variables to factorised output
#'
#' @param x data frame containing admission x-ray variables
#' @param ... further arguments passed to or from other methods
#'
#' @return admission data frame with factor x-ray summary varialbe
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export
#'

cavities_fixer <- function(x, ...) {
  
  # check input
  assert_that(is.data.frame(x))
  
  # useMethod
  UseMethod("cavities_fixer", x)
  
}

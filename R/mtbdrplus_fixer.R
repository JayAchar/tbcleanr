#' Consolidate Hain MTBDRplus results
#'
#' Take laboratory data set and consolidate Hain MTBDRplus results
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

mtbdrplus_fixer <- function(x, ...) {
  
  # check input
  assert_that(is.data.frame(x))
  
  UseMethod("mtbdrplus_fixer", x)
  
}  
 

#' Default method for mtbdrplus_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}

mtbdrplus_fixer.default <- function(x, ...) {
  
  message("No lab object class detected: mtbdrplus_fixer() not applied.")
  x       
}


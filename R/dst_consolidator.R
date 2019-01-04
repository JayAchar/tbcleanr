#' Consolidate DST results
#'
#' Take laboratory data set and consolidate DST results - choose aggregate
#' for categorised results
#' @param x data frame containing variables
#' @param aggregate choose whether to aggregate to categories or retain all drug results
#' @param rm_orig remove original variables - TRUE or FALSE
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export


dst_consolidator <- function(x, aggregate = FALSE, 
                             rm_orig = TRUE) {
  
  # check input
  assert_that(is.data.frame(x))
  
  UseMethod("dst_consolidator", x)
  
}

#' Default method for dst_consolidator()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export 

dst_consolidator.default <- function(x, ...) {
  
  message("No lab object class detected: dst_consolidator() not applied.")
  x        
}

#' Subset lab variables
#'
#' Subset pre-specified TB laboratory variables
#' @param x data frame containing variables
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}

lab_subset.koch6 <- function(x, add = NULL, ...) {
  
  # retain object class
  object_class <- class(x)
  
  # define variables to keep
  keep <- NULL
  
  ## Additional specified variables
  k <- c(keep, add)		# add additional requested variables
  x <- subset(x, select = k)
  
  class(x) <- object_class
  
  x
}
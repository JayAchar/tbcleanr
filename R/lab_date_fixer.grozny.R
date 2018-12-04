#' Aggregate dates in lab database
#'
#' Consolidate sample dates in mycobacterial lab database
#' @param x data frame containing sample date variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @importFrom lubridate is.Date

lab_date_fixer.grozny <- function(x, ...) {
 
# set variables
    vars <- c("dcol1", "dcol2", "dcol3")
 
  # check variables are present
  if (! all(vars %in% names(x))) {
    stop("All date variables not available in data set")
  }
  
  # check all variables are dates
  if (! all(as.logical(lapply(x[ , vars], is.Date)))) {
    stop("All variables are not classed as dates")
  }
  
  # aggregate sample collection date
  x$samp_date <- pmin(x[[vars[1]]], x[[vars[2]]], x[[vars[3]]], na.rm = T)
  
  
  # remove old variables
  x[ , vars] <- NULL
  
  x
}

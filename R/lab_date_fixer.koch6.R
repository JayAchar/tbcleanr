#' Aggregate dates in lab database
#'
#' Consolidate sample dates in mycobacterial lab database
#' @inheritParams lab_date_fixer
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom lubridate is.Date
#' @export 

lab_date_fixer.koch6 <- function(x, ...) {
  
  # set variables
  vars <- "Samplecollectiondate"
  
  # check variables are present
  if (! all(vars %in% names(x))) {
    stop("All date variables not available in data set")
  }
  
  # check all variables are dates
  if (! all(as.logical(lapply(x[ , vars], is.Date)))) {
    stop("All variables are not classed as dates")
  }
  
  # rename sample date variable
  names(x)[names(x) == vars] <- "samp_date"
  
  x
}

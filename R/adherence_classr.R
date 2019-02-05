#' Apply class for adherance data
#'
#' Automatically apply class to adherance data
#' @param x data frame containing variables
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

adherence_classr <- function(x) {
  
  # check arg
  assert_that(is.data.frame(x))
  
  # names of variables in Epiinfo
  epiinfo_varnames <- c("APID", "PHASE", "PH", "NDCLO")
  
  # names of variables in Koch_6
  k6_varnames <- c("RegistrationNb", "MissedDays", "TtrCompletRate", "MBdq")
  
  
  # assign admission file object class
  if (all(epiinfo_varnames %in% names(x))) {
    class(x) <- c(class(x), "epiinfo")
  }
  
  if (all(k6_varnames %in% names(x))) {
    class(x) <- c(class(x), "koch6")
  }
  
  # warning if no object class assigned
  if (! any(c("epiinfo", "koch6") %in% class(x))) 
    warning("Data set not recognised - no object class assigned")
  
  x        
}

#' Apply class for adherance data
#'
#' Automatically apply class to adherance data
#' @param x data frame containing variables
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

adhere_classr <- function(x) {

  # check arg
  assert_that(is.data.frame(x))
  
  # if adherance object class already present - stop further evaluation
  if (any(c("epiinfo", "koch6") %in% class(x))) {
    return(x)
  }

  # names of variables in Epiinfo
  epiinfo_varnames <- c("APID", "PHASE", "PH", "NDCLO")

  # names of variables in Koch_6
  k6_varnames <- c("RegistrationNb", "MissedDays", "TtrCompletRate", "MBdq")


  # assign admission file object class
  if (all(epiinfo_varnames %in% names(x))) {
    class(x) <- c("epiinfo", class(x))
  }

  if (all(k6_varnames %in% names(x))) {
    class(x) <- c("koch6", class(x))
  }

  # warning if no object class assigned
  if (! any(c("epiinfo", "koch6") %in% class(x))) 
    warning("Data set not recognised - no object class assigned")

x
}

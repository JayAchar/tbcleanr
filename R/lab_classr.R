#' Add object class to identify lab database
#'
#' From variable characteristics identify data entry tool and assign
#' object class to identify database for further cleaning
#' @param x data frame containing raw TB admission data
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export
#' 

lab_classr <- function(x) {
  
  # check arg
  assert_that(is.data.frame(x))
  
  # names of variables in Epiinfo
  epiinfo_varnames <- c("APID", "MICRLABN", "BK1", "CULRES", "MGITRES")
  
  # names of variables in Grozny Excel tool
  grozny_varnames <- c("dbno", "micro1", "sampqual2", "mgit1d")
  
  # names of variables in Koch 6
  k6_varnames <- c("RegistrationNb", "Admission", "DSTMethode1", "Lfx3")
  
  # assign admission file object class
  if (all(epiinfo_varnames %in% names(x))) {
    class(x) <- c("epiinfo", class(x))
  }
  
  if (all(k6_varnames %in% names(x))) {
    class(x) <- c("koch6", class(x))
  }
  
  if (all(grozny_varnames %in% names(x))) {
    class(x) <- c("grozny", class(x))
  }
  
  # warning if no object class assigned
  if (! any(c("epiinfo", "koch6", "grozny") %in% class(x))) 
    warning("Data set not recognised - no object class assigned")
  
  x        
}


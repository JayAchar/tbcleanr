#' Add object class to identify admission database
#'
#' From variable characteristics identify data entry tool and assign
#' object class to identify database for further cleaning
#' @param x data frame containing raw TB admission data
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export
#' @examples 
#' # Load EpiInfo admission data
#' epi_raw <- readRDS(system.file("testdata", "epi_adm_classr.rds", package = "tbcleanr"))
#' # Apply adm_classr()
#' epi_new <- adm_classr(epi_raw)
#' # Check class added correctly
#' class(epi_new)
#' 

adm_classr <- function(x) {

# check arg
  assert_that(is.data.frame(x))
  
# names of variables in Epiinfo
  epiinfo_varnames <- c("APID", "BIRTDATE", "Z1", "CONTACTTB")
      
# names of variables in Koch_6
  k6_varnames <- c("registrationnb", "gender", "HowTb", "smearconv")
  
  
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
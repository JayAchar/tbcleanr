#' Subset lab variables
#'
#' Subset pre-specified TB laboratory variables
#' @param x data frame containing variables
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}


lab_subset.epiinfo <- function(x, add = NULL, ...) {
  
  # retain object class
  object_class <- class(x)
  
  # define variables to keep
  keep <- c("APID", "MICRLABN", "FIRST", "SECOND", "THIRD", "BK1", "BK2", "BK3",
            "RES", "RES02", "RES03", "RES04", "RESULT", "RESULT02", "RESULT03",
            "RESULT04", "MGITH", "MGITE", "MGITR", "MGITZ", "HAINH",
            "HAINR", "H", "E", "R", "Z", "KM", "OF", "CAP",
            "H1", "E1", "R1", 
            "Z1", "KM1", "OF1", "CAP1",
            "MFX1", "GX_res1", "GX_res2", "GX_res3", "GX_res4")	
  
  ## Additional specified variables
  k <- c(keep, add)		# add additional requested variables
  x <- subset(x, select = k)
  
  class(x) <- object_class
  
  x
}

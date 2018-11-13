#' Subset lab variables
#'
#' Subset pre-specified TB laboratory variables
#' @param x data frame containing variables
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}


lab_subset.grozny <- function(x, add = NULL, ...) {
  
  # retain object class
  object_class <- class(x)
  
  # define variables to keep
  keep <- c("dbno", "dstno", "dob", "sputum", "dcol1", "dcol2", "dcol3",
            "micro1", "micro2", "micro3", "xpert1err", "xpert2err", "xpert1res",
            "xpert2res", "xpert1rif", "xpert2rif", "mgitres",
            "ms", "mr", "mh", "mz", "me", "mcm", "mam", "mlfx", "ljres", 
            "ljs", "ljr", "ljh", "ljz", "lje",
            "ctmicres", "ctmgitres",
            "cts", "ctr", "cth", "ctz", "cte", "ctcm", "ctam", "ctlfx", "ctmfx",
            "ctmfx2", "ctlzd", "cthres", "cthrifres", "cthinhres")
  
  ## Additional specified variables
  k <- c(keep, add)		# add additional requested variables
  x <- subset(x, select = k)
  
  class(x) <- object_class
  
  x
}

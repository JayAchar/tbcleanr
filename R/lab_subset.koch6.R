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
  keep <- c("RegistrationNb",
            "Samplecollectiondate", 
            "SmearResult", "CultResult", 
            "GeneXpertResult", "HainResult", "DSTLabNumber1", 
            "DSTMethode1", "DSTResultDate1", "E1", "H1", "R1", "Z1", "Am1", 
            "Cm1", "Km1", "S1", "Vi1", "Cfx1", "Gfx1", "Lfx1", "Mfx1", "Ofx1", 
            "Cs1", "Eto1", "PAS1", "PAS Na1", "Pto1", "Trd1", "Amx-Clv1", 
            "Bdq1", "Cfz1", "Clr1", "Dld1", "hdH1", "ImpCln1", "Lzd1", "Mpm1", 
            "Thz1", "DSTLabNumber2", "DSTMethode2", "DSTResultDate2", 
            "E2", "H2", "R2", "Z2", "Am2", "Cm2", "Km2", "S2", "Vi2", "Cfx2", 
            "Gfx2", "Lfx2", "Mfx2", "Ofx2", "Cs2", "Eto2", "PAS2", "PAS Na2", 
            "Pto2", "Trd2", "Amx-Clv2", "Bdq2", "Cfz2", "Clr2", "Dld2", "hdH2", 
            "ImpCln2", "Lzd2", "Mpm2", "Thz2", "DSTLabNumber3", 
            "DSTMethode3", "DSTResultDate3", "E3", "H3", "R3", "Z3", "Am3", 
            "Cm3", "Km3", "S3", "Vi3", "Cfx3", "Gfx3", "Lfx3", "Mfx3", "Ofx3", 
            "Cs3", "Eto3", "PAS3", "PAS Na3", "Pto3", "Trd3", "Amx-Clv3", 
            "Bdq3", "Cfz3", "Clr3", "Dld3", "hdH3", "ImpCln3", "Lzd3", "Mpm3", 
            "Thz3", "DSTLabNumber4", "DSTMethode4", "DSTResultDate4", 
            "E4", "H4", "R4", "Z4", "Am4", "Cm4", "Km4", "S4", "Vi4", "Cfx4", 
            "Gfx4", "Lfx4", "Mfx4", "Ofx4", "Cs4", "Eto4", "PAS4", "PAS Na4", 
            "Pto4", "Trd4", "Amx-Clv4", "Bdq4", "Cfz4", "Clr4", "Dld4", "hdH4", 
            "ImpCln4", "Lzd4", "Mpm4", "Thz4", "DSTLabNumber5", 
            "DSTMethode5", "DSTResultDate5", "E5", "H5", "R5", "Z5", "Am5", 
            "Cm5", "Km5", "S5", "Vi5", "Cfx5", "Gfx5", "Lfx5", "Mfx5", "Ofx5", 
            "Cs5", "Eto5", "PAS5", "PAS Na5", "Pto5", "Trd5", "Amx-Clv5", 
            "Bdq5", "Cfz5", "Clr5", "Dld5", "hdH5", "ImpCln5", "Lzd5", "Mpm5", 
            "Thz5")
  
  ## Additional specified variables
  k <- c(keep, add)		# add additional requested variables
  x <- subset(x, select = k)
  
  class(x) <- object_class
  
  x

}


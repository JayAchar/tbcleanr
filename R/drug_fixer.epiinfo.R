#' Fix EpiInfo drug dosing data
#'
#' Take data frame with drug dosing variables and output 
#' binary factorised variables 
#' @param x data frame containing outcome variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at

drug_fixer.epiinfo <- function(x, ...) {
    
    # check drug variables correct based on set arg
    drugs <- c("HDH","RDR", "EDE", "ZDZ", "SDS",
           "KADKA", "OFLDOFL", "CAPDCAP", "ETHDETH", "CYCLDCYCL", "AMXDAMX", "PASDPAS",
           "CLADCLA", "CLODCLO", "LXDLX", "MXDMX", "PTDPT", "LZDDLZD", "IMPDIMP",
           "BDQDBDQ", "DLMDDLM")
    
    # convert all variables to numerical for yn_binary_fixer()
    x[] <- map_at(x, .at = drugs, .f = as.numeric)
    
    # recode drug variables 
    x[] <- map_at(x, .at = drugs, .f = yn_binary_fixer)
    
x    
}
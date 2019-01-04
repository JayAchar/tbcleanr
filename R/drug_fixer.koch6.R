#' Fix Koch 6 drug dosing data
#'
#' Take data frame with drug dosing variables and output 
#' binary factorised variables 
#' @param x data frame containing outcome variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at
#' @export 

drug_fixer.koch6 <- function(x, ...) {
    
    # check drug variables correct based on set arg
    drugs <- c("E", "H", "R", "Z", "Am", "Cm", "Km", "Lfx", "Mfx", 
               "Ofx", "Cs", "Eto", "PAS", "PAS Na", "Pto", "Amx-Clv",
               "Bdq", "Cfz", "Clr", "Dld", "hdH", "ImpCln", "Lzd", "Mpm")
    
    # recode drug variables 
    x[] <- map_at(x, .at = drugs, .f = yn_binary_fixer)
    
x
}

#' Convert admission binary variables to factorised output
#'
#' Take admission data frame with binary variables and output 
#' binary factorised variables 
#' @param x data frame containing admission binary variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at
#' @export 

binary_fixer.epiinfo <- function(x, ...) {
    
    
    # define variables of interest
    # numerical variables
    v <- c("DIABETES","CARDIODI", "RENALFAI","PSYCHI", "SEIZURE", "HEPADIS",
           "ALCO")
    # character variables
    chr_var <- c("HD", "EE", "RR", "ZP","CSC", "SMS", "AMA", "KMK", "CPX", "OFX",
                 "TT", "ETHE","PASP", "AMXC","CFZ", "CLRC","CMC", "OTH", "EVER",
                 "INJECT", "HOMELESS", "HEALTHWO", "PRIWO", "TOBACCO")
    
    # check all numerical variables exist in data frame
    if (! all(v %in% names(x))) {
        warning("All binary variables not included in data frame")
    }
    
    # recode numerical variables 
    x[] <- map_at(x, .at = v, .f = yn_binary_fixer)
    
    
    # recode character variables
    x[] <- map_at(x, .at = chr_var, .f = yn_binary_fixer)

x

}

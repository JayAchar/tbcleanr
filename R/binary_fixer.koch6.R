#' Convert admission binary variables to factorised output
#'
#' Take admission data frame with binary variables and output 
#' binary factorised variables 
#' @param x data frame containing admission binary variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at

binary_fixer.koch6 <- function(x, ...) {
    
    # define variables of interest
    v <- c("diabetes", "cardiodi", "renalfail")

    # check all numerical variables exist in data frame
    if (! all(v %in% names(x))) {
        warning("All binary variables not included in data frame")
    }
    
    
    # recode numerical variables 
    x[] <- map_at(x, .at = v, .f = yn_binary_fixer)  
    
x
}



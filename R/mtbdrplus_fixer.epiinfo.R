#' Consolidate Hain MTBDRplus results
#'
#' Take laboratory data set and consolidate Hain MTBDRplus results
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @importFrom purrr map_at
#' @importFrom dplyr case_when
#' @export 

mtbdrplus_fixer.epiinfo <- function(x, ...) {
  
  # set variable names
  h_resist <- c("HAINH", "HAINR")
  
  # check variables present in data
  if (! all(h_resist %in% names(x))) {
    warning("MTBDRplus variables not present in data - mtbdrplus_fixer() not applied")
    return(x)
  }
  
  # recode and factorise hain resistance results
  x[] <- map_at(x, .at = h_resist, .f = 
                  ~ case_when(.x == 2 ~ 0L,
                              .x == 1 ~ 1L, 
                              TRUE ~ NA_integer_) %>% 
                  factor(levels = 0:1, labels = c("Sensitive", "Resistant")))
  
  # rename resistance vars
  colnames(x)[match(h_resist, colnames(x))] <- c("hain_inh", "hain_rif")
  
x  
}


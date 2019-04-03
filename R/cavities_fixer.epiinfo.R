#' Convert admission x-ray variables to factorised output
#'
#' @param x data frame containing xray variable data
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom dplyr mutate
#' @importFrom magrittr %>% 
#' @return data frame with two new factor variables indicating
#' whether a x-ray is normal or abnormal, and whether it shows
#' a cavity or not
#' @export


cavities_fixer.epiinfo <- function(x, 
                           rm_orig = FALSE, 
                           ...) {
  
  # XRAYRES: xray findings
    ## convert unknown and not done to NA
    x$XRAYRES[x$XRAYRES %in% c(3, 4)] <- NA_integer_
  
    
  # ABNORM: cavities present
    
    ## convert variables to factor
    x <- x %>% 
      mutate(xray_result = factor(XRAYRES, 
                              levels = c(1, 2),
                              labels = c("Normal",
                                         "Abnormal")),
             cavity = factor(ABNORM,
                             levels = c(1, 2),
                             labels = c("Cavitary", 
                                        "Non-cavitary")))
  
  # remove original variables
  if (rm_orig) {
    x$XRAYRES <- NULL
    x$ABNORM <- NULL
  }
  
  
  x
}

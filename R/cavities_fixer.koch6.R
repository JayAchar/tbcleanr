#' Convert admission x-ray variables to factorised output
#'
#' @param x data frame containing xray variable data
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom dplyr mutate
#' @importFrom magrittr %>% 
#' @importFrom rlang .data
#' @return data frame with two new factor variables indicating
#' whether a x-ray is normal or abnormal, and whether it shows
#' a cavity or not
#' @export


cavities_fixer.koch6 <- function(x, 
                           rm_orig = FALSE, 
                           ...) {
  
  # XRAYRES: xray findings
    ## convert unknown and not done to NA
    x$XRay_Finding[x$XRay_Finding == 0L] <- NA_integer_
  
    
  # ABNORM: cavities present
    # presence of cavity data entry only possible if XRay_Finding
    # is abnormal in Koch6
    
  # cavsimple: simplified description of abnormality
    x$cavsimple[x$cavsimple == 0L] <- NA_integer_
    
    ## convert variables to factor
    x <- x %>% 
      mutate(xray_result = factor(.data$XRay_Finding, 
                              levels = c(1, 2),
                              labels = c("Normal",
                                         "Abnormal")),
             cavity = case_when(.data$cavsimple == 1L|
                                .data$cav %in% c(1, 2) |
                                .data$cavD %in% c(1, 2) ~ 1L,
                                .data$cavsimple == 2L |
                                  (.data$cav == 0L &
                                     .data$cavD ==  0L) ~ 2L, 
                                TRUE ~ NA_integer_),
             cavity = factor(.data$cavity,
                             levels = c(1, 2),
                             labels = c("Cavitary", 
                                        "Non-cavitary")))
  
  # remove original variables
  if (rm_orig) {
    x$XRay_Finding <- NULL
    x$cavsimple <- NULL
    x$cav <- NULL
    x$cavD <- NULL
  }

  x
}

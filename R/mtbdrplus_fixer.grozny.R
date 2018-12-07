#' Consolidate Hain MTBDRplus results
#'
#' Take laboratory data set and consolidate Hain MTBDRplus results
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @importFrom purrr map_at
#' @importFrom dplyr case_when mutate recode
#' @importFrom rlang .data

mtbdrplus_fixer.grozny <- function(x, ...) {
  # save original class
  c_start <- class(x)
  
  # set variable names
  hain_vars <- c("cthres", "cthrifres", "cthinhres")
  h_resist <- c("cthinhres", "cthrifres")
  
  # check variables present in data
  if (! all(h_resist %in% names(x))) {
    warning("MTBDRplus variables not present in data - mtbdrplus_fixer() not applied")
    return(x)
  }
  
  # recode and factorise hain resistance results
  x[] <- map_at(x, .at = h_resist, .f = 
                  ~ case_when(.x == "S" ~ 0L,
                              .x == "R" ~ 1L, 
                              TRUE ~ NA_integer_) %>% 
                  factor(levels = 0:1, labels = c("Sensitive", "Resistant")))
  
  # rename resistance vars
  colnames(x)[match(h_resist, colnames(x))] <- c("hain_inh", "hain_rif")
  
  # recode hain result
  x <- x %>% 
    mutate(hain_res = recode(.data$cthres, "Pos" = 1L, "Neg" = 0L) %>% 
             factor(levels = 0:1, labels = c("Negative", "Positive"))) 
  
  x$cthres <- NULL

  # reapply class
  class(x) <- c_start
  
  x  
}


#' Consolidate smear and culture results
#'
#' Take laboratory data set and consolidate repeated results to give summary variable
#' @param x data frame containing variables
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect

result_consolidator.epiinfo <- function(x, rm_orig = TRUE, ...) {
  
  # define smear vars
  smear_vars <- c("BK1", "BK2", "BK3")
  
  # define culture vars
  culture_vars <- c("RES", "RES02", "RES03", "RES04", "RESULT", 
                    "RESULT02", "RESULT03", "RESULT04")
  
  ### consolidate smear results
  # recode smear results to numeric
  x[] <- map_at(x, .at = smear_vars, .f = 
                  ~ case_when(.x == 0 ~ 0L,
                              .x == 5 ~ 1L,
                              .x == 1 ~ 2L, 
                              .x == 2 ~ 3L,
                              .x == 3 ~ 4L,
                              TRUE ~ NA_integer_))
  
  # find maximum smear for each sample
  x$smear <- do.call(pmax, c(x[ , smear_vars], na.rm = T))
  
  # factorise and order smear variable
  x$smear <- factor(x$smear, levels = 0:4,
                    labels = c("Negative", "Scanty", "1+", "2+", "3+"),
                    ordered = T)
  
  # remove original smear variables
  if (rm_orig) x[, smear_vars] <- NULL
  
  ### consolidate culture results
  # recode culture results to numeric
  x[] <- map_at(x, .at = culture_vars, .f = 
                  ~ as.numeric(.x) %>% 
                  case_when(.x == 1, 0L,
                            .x == 2, 1L,
                            TRUE ~ NA_integer_))
  
  # find maximum culture for each sample
  x$culture <- do.call(pmax, c(x[ , culture_vars], na.rm = T))
  
  # factorise and order culture variable
  x$culture <- factor(x$culture, levels = 0:1,
                      labels = c("Negative", "Positive"),
                      ordered = T)
  
  # remove original variables
  if (rm_orig) x[, culture_vars] <- NULL
x
}

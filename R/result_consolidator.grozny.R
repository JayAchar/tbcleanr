#' Consolidate smear and culture results
#'
#' Take laboratory data set and consolidate repeated results to give summary variable
#' @inheritParams result_consolidator
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @export 

result_consolidator.grozny <- function(x, rm_orig = TRUE, ...) {
  
  # define smear vars
  smear_vars <- c("micro1", "micro2", "micro3", "ctmicres")
  
  # define culture vars
  culture_vars <- c("mgitres", "ljres", "ctmgitres")
  
  ### consolidate smear results
  # recode smear results to numeric
  x[] <- map_at(x, .at = smear_vars, .f = 
                  ~ case_when(.x == "Negative" ~ "0",
                              .x == "Scanty" ~ "1",
                              .x == "1+" ~ "2", 
                              .x == "2+" ~ "3",
                              .x == "3+" ~ "4",
                              TRUE ~ NA_character_) %>% 
                  as.numeric())
  
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
                  ~ case_when(str_detect(.x, "Neg") ~ "0",
                              str_detect(.x, "Pos") ~ "1",
                              TRUE ~ NA_character_) %>% 
                  as.numeric())

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

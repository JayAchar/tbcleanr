#' Clean treatment history variable
#'
#' @param x data frame of TB admission data
#'
#' @return data frame with factorised treatment history
#' variable
#' @importFrom assertthat assert_that
#' @importFrom forcats fct_recode
#' @export

txhistory.epiinfo <- function(x) {
  # check variable present in data frame
  assert_that("REGRP" %in% names(x))
  
  # check for results outside of definition
  all(unique(x$REGRP) %in% 1:8)
  
  # factorise variable
  x$REGRP <- factor(x$REGRP, levels = 1:8, 
                    labels = c("New",
                               "Relapse",
                               "Tx after LTFU",
                               "Tx after failure",
                               "Tx after failure Cat II",
                               "Transfer in",
                               "Other previously treated", 
                               "Amplified after failure")) %>% 
    # merge levels which are not defined by WHO 2013
    forcats::fct_recode("Tx after failure" = "Tx after failure Cat II")
  
  x
}

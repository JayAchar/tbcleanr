#' Clean treatment history variable
#'
#' @param x data frame of TB admission data
#'
#' @return data frame with factorised treatment history
#' variable
#' @importFrom assertthat assert_that
#' @export

txhistory.koch6 <- function(x) {
  # check variable present in data frame
  assert_that("cdhistory" %in% names(x))
  
  # check for results outside of definition
  all(unique(x$cdhistory) %in% 0:5)
  
  # convert "not defined" to NA
  x$cdhistory[x$cdhistory == 0] <- NA_integer_
  
  # factorise variable
  x$cdhistory <- factor(x$cdhistory, levels = 1:5, 
                    labels = c("New",
                               "Relapse",
                               "Tx after failure",
                               "Tx after LTFU",
                               "Other previously treated"))

  x
}

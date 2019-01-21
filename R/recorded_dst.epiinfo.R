#' Clean recorded baseline DST variable
#'
#' @param x data frame of TB admission data
#'
#' @return data frame with factorised baseline DST
#' @importFrom assertthat assert_that
#' @export

recorded_dst.epiinfo <- function(x) {
  # check variable present in data frame
  assert_that("DIPRO" %in% names(x))
  
  # check for results outside of definition
  assert_that(all(unique(x$DIPRO) %in% c(1:7, 99, NA_integer_)))
  
  # convert missing to NA
  x$DIPRO[x$DIPRO == 99] <- NA_integer_
  
  # factorise variable
  x$DIPRO <- factor(x$DIPRO, levels = 1:7, 
                    labels = c("HR sensitive",
                               "H/R mono-resistant",
                               "PDRTB",
                               "MDRTB",
                               "XDRTB",
                               "Unknown",
                               "Late detection of resistance")) 
  
  x
}

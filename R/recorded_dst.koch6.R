#' Clean recorded baseline DST variable
#'
#' @param x data frame of TB admission data
#'
#' @return data frame with factorised baseline DST
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @importFrom magrittr %>% 
#' @importFrom dplyr filter
#' @importFrom forcats fct_recode
#' @export

recorded_dst.koch6 <- function(x) {
  
  # check variable present in data frame
  assert_that(all(c("cdstrainprofil", 
                "cdstrainconf") %in% names(x)))
  
  # check for results outside of definition
  assert_that(all(unique(x$cdstrainprofil) %in% 0:4))
  assert_that(all(unique(x$cdstrainconf) %in% 0:7))
  
  
  # convert missing to NA
  x$cdstrainprofil[x$cdstrainprofil %in% c(0, 3)] <- NA_integer_
  
  x$cdstrainconf[x$cdstrainconf == 0] <- NA_integer_
  
  # warning if strain is DS but has additional 2nd line resistance results
  if (x %>% 
        filter(.data$cdstrainprofil == 1 &
                   .data$cdstrainconf > 0) %>% 
        nrow() > 0) {
    warning("Recorded DST variable error likely detected. Suggest manual review of raw data.")
  }
  
  # factorise variable
  x$recorded_dst <- x$cdstrainconf
  x$recorded_dst[x$cdstrainprofil == 1] <- 0L
  x$recorded_dst[x$cdstrainprofil == 4 & is.na(x$cdstrainconf)] <- 7L
  
  x$recorded_dst <- factor(x$recorded_dst, levels = 0:7, 
                           labels = c("DSTB",
                                      "Inh mono",
                                      "HE resistant",
                                      "RRTB",
                                      "Xpert RRTB",
                                      "MDRTB",
                                      "XDRTB",
                                      "Other")) %>% 
  
  # merge identical levels
  forcats::fct_recode("RRTB" = "Xpert RRTB")
  
  x
}

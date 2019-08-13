#' Clean drug adherence data
#'
#' Take drug adherence data from routine TB programmes and clean for 
#' further analysis.
#' 
#' @inheritParams adhere_cleanr
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% select starts_with
#' @importFrom lubridate dmy
#' @importFrom rlang .data
#' @export

adhere_cleanr.epiinfo <- function(x) {

  # check arguments  
  assert_that(is.data.frame(x))

  # remove variables
  vars_remove <- c("SURNAME", "NAME", "BIRTDATE", "SEX", "STARTTRE", 
                   "MEDTT", "MEDTTT", "MEDTTD")
  
  x[, vars_remove] <- NULL
  
    
  # x <- x %>% 
  #   # remove identifiable data
  #   select(-.data$SURNAME, -.data$NAME, -.data$BIRTDATE, -starts_with("MEDTT")) %>% 
  #   # convert dates
  #   mutate(STARTTRE = dmy(.data$STARTTRE)) %>% 
  #   # inpatient or outpatient to factor
  #   mutate(PHASE = factor(.data$PHASE, 
  #                          levels = c("OUT", "IN"),
  #                          labels = c("Outpatient", "Inpatient")))

  
x
}

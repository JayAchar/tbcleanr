#' Clean drug adherence data
#'
#' Take drug adherence data from routine TB programmes and clean for 
#' further analysis.
#' 
#' @param x data frame containing drug adherence data
#' @param adm optional arugement defining admission data frame to 
#' force function to nest adherence data. 
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% select starts_with
#' @importFrom lubridate dmy
#' @importFrom rlang .data
#' @export

adhere_cleanr.epiinfo <- function(x,
                                  adm = NULL) {
# check arguments  
  assert_that(is.data.frame(x))
  assert_that(is.null(adm) | is.data.frame(adm))
  

  x <- x %>% 
    # remove identifiable data
    select(-.data$SURNAME, -.data$NAME, -.data$BIRTDATE, -starts_with("MEDTT")) %>% 
    # convert dates
    mutate(STARTTRE = dmy(.data$STARTTRE)) %>% 
    # inpatient or outpatient to factor
    mutate(PHASE = factor(.data$PHASE, 
                           levels = c("OUT", "IN"),
                           labels = c("Outpatient", "Inpatient")))

  
x
}

#' Clean drug adherence data
#'
#' Take drug adherence data from routine TB programmes and clean for 
#' further analysis.
#' 
#' @inheritParams adhere_cleanr
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% left_join
#' @importFrom purrr map reduce
#' @export

adhere_cleanr.epiinfo <- function(x) {

  # check arguments  
  assertthat::assert_that(is.data.frame(x))

  # remove variables
  vars_remove <- c("SURNAME", "NAME", "BIRTDATE", "SEX", "STARTTRE", 
                   "MEDTT", "MEDTTT", "MEDTTD", "PHASE")
  
  x[, vars_remove] <- NULL
  
  # all drug names
  drug_names <- c("H", "R", "E", "Z", "S", "KA", "CAP", "OFL", "ETH", "CYC", 
                  "PAS", "AMX", "CLA", "CLO", "MFX", "PTO", "LFX", "LZD", "BDQ", 
                  "IMP", "DLM")
  
  # generate data frame of monthly adherence percentage by drug
  out <- purrr::map(drug_names, .f = ~drug_adhere(x, drug = .x)) %>% 
    purrr::reduce(dplyr::left_join, by = c("APID", "tx_month"))
    
  out
}

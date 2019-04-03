#' Clean TB admission data sets
#'
#' Simplify workflow of cleaning TB admission data sets from EpiInfo or Koch 6
#' by adding an object class for identification, subsetting variables, 
#' extracting additional information from ID numbers in Epiinfo, formatting date, 
#' gender, HIV and treatment outcome variables. Also, converts drug dosing variables
#' to binary variables
#' @param x data frame containing variables
#' @param add string of additional variable names to retain in cleaned output data frame
#' @param ... further arguments passed to or from other methods
#' @return Data frame with an object attribute signifying the data collection software - 
#' "epiinfo" or "koch6"
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @export



adm_data_cleanr <- function(x, add = NULL, ...) {
# check input
    assert_that(is.data.frame(x))

# =======================================================
	x <- x %>%
			# subset variables
		adm_subset(add = add, ...) %>%
			# detangle apid number
		id_detangle() %>%
			# date format
		date_format(...) %>%
			# categorise gender variable
		gender_fixer() %>%
			# hiv variables consolidated
		hiv_fixer() %>%
	    # treatment history 
	  txhistory() %>% 
	    # programme entered DST
	  recorded_dst() %>% 
			# cavities variables consolidated
    cavities_fixer() %>%
			# fix outcomes variables
		outcome_fixer(...) %>%
			# change all drugs from doses to binary
		drug_fixer() %>%
			# change all binary variables to factors
		binary_fixer(...)	

x
}



#' Cleans TB admissions data sets
#'
#' Take admission data set and perform multiple adjustments based on which
#' database set is being used
#' @param x data frame containing variables
#' @param add string of additional variable names to retain in cleaned output data frame
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @export


adm_data_cleanr <- function(x, add = NULL, ...) {
# check input
    assert_that(is.data.frame(x))

# =======================================================
	x <- x %>%
	        # add object class
	    adm_classr() %>% 
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
			# cavities variables consolidated
#		cavities_fixer(software = software, project = project, ...) %>%
			# fix outcomes variables
		outcome_fixer(...) %>%
			# change all drugs from doses to binary
		drug_fixer() %>%
			# change all binary variables to factors
		binary_fixer(...)	

x
}



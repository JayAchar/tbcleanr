#' Cleans TB admissions data sets
#'
#' Take admission data set and perform multiple adjustments based on which
#' database set is being used
#' @param x data frame containing variables
#' @param set define admission data set being used. Values can be - "k6_adm_standard".
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @import magrittr
#' @export
#' @examples
#' \dontrun{
#' adm_data_cleanr(p, lab = "chechnya")
#' }


adm_data_cleanr <- function(x, set = "k6_adm_standard") {
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}
# acceptable values for lab
	allowed <- c("k6_adm_standard")

# check lab arg is within acceptable values
	if (! set %in% allowed) {
		stop("Specify lab argument within specified values")
	}

# =======================================================
	# clean and convert data frame

	if (set == "k6_adm_standard") {
	x <- x %>%
			# subset variables
		subset_vars(set = "k6_adm_standard") %>%
			# detangle apid number
		k6_idno_fixer() %>%
			# bmi generator
		bmi_generator() %>%
			# date format
		date_format() %>%
			# categorise gender variable
		gender_fixer() %>%
			# hiv variables consolidated
		hiv_fixer() %>%
			# cavities variables consolidated
		k6_cavities_fixer() %>%
			# fix outcomes variables
		k6_outcome_fixer() %>%
			# change all drugs from doses to binary
		drug_fixer() %>%
			# change all binary variables to factors
		adm_binary_fixer()	
	}


x
}



#' Cleans TB admissions data sets
#'
#' Take admission data set and perform multiple adjustments based on which
#' database set is being used
#' @param x data frame containing variables
#' @param set define admission data set being used. Values can be - "k6_adm_standard".
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' adm_data_cleanr(p, lab = "chechnya")
#' }


adm_data_cleanr <- function(x, set = "k6_adm_standard", ...) {
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}
# acceptable values for lab
	allowed <- c("k6_adm_standard", "nukus_epi_info")

# check set arg is within acceptable values
	if (! set %in% allowed) {
		stop("Specify set argument within specified values")
	}

# =======================================================
	# clean and convert data frame

	if (set == "k6_adm_standard") {
	x <- x %>%
			# subset variables
		subset_vars(set = "k6_adm_standard") %>%
			# detangle apid number
		id_detangle(db = "k6") %>%
			# date format
		date_format() %>%
			# categorise gender variable
		gender_fixer() %>%
			# hiv variables consolidated
		hiv_fixer() %>%
			# cavities variables consolidated
		cavities_fixer(db = "k6") %>%
			# fix outcomes variables
		outcome_fixer(db = "k6") %>%
			# change all drugs from doses to binary
		drug_fixer() %>%
			# change all binary variables to factors
		adm_binary_fixer(set = "k6_adm_standard")	
	}


	if (set == "nukus_epi_info") {
	x <- x %>%
			# subset variables
		subset_vars(set = "nukus_epi_info") %>%
			# detangle apid number
		id_detangle(db = "epi_info") %>%
			# date format
		date_format() %>%			
			# categorise gender variable
		gender_fixer(db = "epi_info") %>%	
			# hiv variables consolidated
		hiv_fixer(db = "epi_info") %>%
			# cavities variables consolidated
		cavities_fixer(db = "epi_info") %>%
			# fix outcomes variables
		outcome_fixer(db = "epi_info") %>%
			# change all drugs from doses to binary
		drug_fixer(set = "nukus_epi_info") %>%
			# change all binary variables to factors
		adm_binary_fixer(set = "nukus_epi_info") %>%
			# adjust miscellaneous epi info variables
		epi_info_misc_cleanr()
	}

x
}



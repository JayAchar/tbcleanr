#' Cleans TB admissions data sets
#'
#' Take admission data set and perform multiple adjustments based on which
#' database set is being used
#' @param x data frame containing variables
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @export


adm_data_cleanr <- function(x, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"), 
								add = NULL, ...) {
# check input
    assert_that(is.data.frame(x))

# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)

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
		drug_fixer(software = software, project = project, file = file, ...) %>%
			# change all binary variables to factors
		adm_binary_fixer(software = software, project = project, file = file, ...)	

	if (software == "epiinfo") {
		x <- x %>%
			epi_info_misc_cleanr()
	} 

# adjust dstnumber var to match lab dstnumber var
	if (software == "koch_6") {
		x$dstnumber <- as.numeric(x$dstnumber)
		names(x)[names(x) == "dstnumber"] <- "dstno"
	}


x
}



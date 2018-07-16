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
#' @export
#' @examples
#' \dontrun{
#' adm_data_cleanr(p, set = "k6_adm_standard")
#' }


adm_data_cleanr <- function(x, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"), 
								add = NULL, ...) {
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)

# =======================================================
	x <- x %>%
			# subset variables
		subset_vars(software = software, project = project, file = file, add = add, ...) %>%
			# detangle apid number
		id_detangle(software = software, ...) %>%
			# date format
		date_format(...) %>%
			# categorise gender variable
		gender_fixer(software = software, ...) %>%
			# hiv variables consolidated
		hiv_fixer(software = software, ...) %>%
			# cavities variables consolidated
		cavities_fixer(software = software, ...) %>%
			# fix outcomes variables
		outcome_fixer(software = software, ...) %>%
			# change all drugs from doses to binary
		drug_fixer(software = software, project = project, file = file, ...) %>%
			# change all binary variables to factors
		adm_binary_fixer(software = software, project = project, file = file, ...)	

	if (software == "epiinfo") {
		x <- x %>%
			epi_info_misc_cleanr()
	}


x
}



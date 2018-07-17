#' Cleans laboratory data sets
#'
#' Take laboratory data set and perform multiple adjustments based on which
#' laboratory data set is being used
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
#' lab_data_cleanr(p, lab = "chechnya")
#' }


lab_data_cleanr <- function(x, software = c("excel", "koch_6", "epiinfo"),
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
	# clean and convert data frame
	x <- x %>%
			# subset all vars required
		subset_vars(software = software, project = project, file = file, add = add, ...) %>%
			# find and format all dates
		date_format(...) %>%
			# detangle dstno
		dstno_detangle(software = software, project = project, file = file, ...) %>%
			# detangle idno
		id_detangle(software = software, project = project, file = file, ...) %>%
			# consolidate sample date
		lab_date_consolidator(software = software, project = project, file = file, ...) %>%
			# consolidate xpert results
		xpert_result_fixer(software = software, project = project, file = file, 
							rm_orig = TRUE, ...) %>%
			# fix lab samples variable
		lab_sample_fixer(software = software, project = project, file = file, 
							rm_orig = TRUE, ...) %>%
			# consolidate smear results
		result_consolidator(software = software, project = project, file = file,
							test = "smear", rm_orig = TRUE, ...) %>%
			# consolidate culture results
		result_consolidator(software = software, project = project, file = file,
							test = "culture", rm_orig = TRUE, ...) %>%
			# consolidate DST results
		dst_consolidator(software = software, project = project, file = file,
							aggregate = FALSE, rm_orig = TRUE, ...) %>%
			# consolidate hain mtbdrplus results
		mtbdrplus_fixer(software = software, project = project, file = file) %>%
			# convert all zeros to NA in continuous variables
		zero_to_na(software = software, project = project, file = file, ...) %>%
			# convert from wide to long format and remove all results == NA
		lab_longr(software = software, project = project, file = file, ...)			


if (software == "excel" && project == "chechnya" && file == "lab") {
	# check variable names are all present
		vars <- c("dbno", "dstno", "dob", "samp_date", "sample", "smear", "culture",
					"xpert_res", "xpert_rif", "hain_res", "hain_rif", "hain_inh", 
					"dst_p_rif", "dst_p_inh")

} else if (software == "excel" && project == "kk" && file == "clinical_lab") {
	# check variable names are all present
		vars <- c("district", "id", "ds_dr", "date", "test", "result", "comm")
	
} else if (software == "koch_6" && file == "clinical_lab") {
		
		vars <- c("registrationnb", "labclindate", "test", "result")

} else {
		return(x)
	}

# check specified vars present in output dataframe
	if (! (all(names(x) %in% vars))) {
			warning("All variables in final data frame are not recognised")
		}

# reorder variables in data frame
		x <- x[ ,c(vars, setdiff(colnames(x), vars))]

x
}
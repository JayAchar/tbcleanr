#' Cleans laboratory data sets
#'
#' Take laboratory data set and perform multiple adjustments based on which
#' laboratory data set is being used
#' @param x data frame containing variables
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @export

lab_data_cleanr <- function(x, add = NULL, ...) {

  # check input
  assert_that(is.data.frame(x))
  

# =======================================================
	# clean and convert data frame
	x <- x %>%
	  # add object attribute for lab data collection tool
	  lab_classr() %>% 
			# subset all vars required
		lab_subset(...) %>%
			# find and format all dates
		date_format(...) %>%
			# consolidate sample date
		lab_date_fixer(...) %>%
			# consolidate xpert results
		xpert_result_fixer(rm_orig = TRUE, ...) %>%
			# fix lab samples variable
		lab_sample_fixer(rm_orig = TRUE, ...) %>%
			# consolidate smear results
		result_consolidator(rm_orig = TRUE, ...) %>%
			# consolidate DST results
		dst_consolidator(software = software, project = project, file = file,
							aggregate = FALSE, rm_orig = TRUE, ...) %>%
			# consolidate hain mtbdrplus results
		mtbdrplus_fixer(software = software, project = project, file = file) %>%
			# convert all zeros to NA in continuous variables
		zero_to_na(software = software, project = project, file = file, ...) %>%
			# convert from wide to long format and remove all results == NA
		lab_longr(software = software, project = project, file = file, ...)			


if (software == "excel" & project == "chechnya" & file == "lab") {
	# check variable names are all present
		vars <- c("dbno", "dstno", "dob", "samp_date", "sample", "smear", "culture",
					"xpert_res", "xpert_rif", "hain_res", "hain_rif", "hain_inh", 
					"dst_p_rif", "dst_p_inh")

} else if (software %in% c("excel", "epiinfo") & project == "kk" & file == "clinical_lab") {
	# check variable names are all present
		vars <- c("district", "id", "ds_dr", "date", "test", "result", "comm")
	
} else if (software == "koch_6" & file == "clinical_lab") {
		
		vars <- c("id", "labclindate", "test", "result")

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

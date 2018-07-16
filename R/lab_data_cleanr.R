#' Cleans laboratory data sets
#'
#' Take laboratory data set and perform multiple adjustments based on which
#' laboratory data set is being used
#' @param x data frame containing variables
#' @param lab define laboratory dat set being used. Values can be - "chechnya",
#' "nukus_clin_lab".
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' lab_data_cleanr(p, lab = "chechnya")
#' }


lab_data_cleanr <- function(x, lab, ...) {
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}
# acceptable values for lab
	l <- c("chechnya_myco_lab", "nukus_clin_lab", "k6_clin_lab",
			"nukus_myco_lab")

# check lab arg is within acceptable values
	if (! lab %in% l) {
		stop("Specify lab argument within specified values")
	}

# =======================================================
	# clean and convert data frame

if (lab == "chechnya_myco_lab") {
	x <- x %>%
			# subset all vars required
		subset_vars(set = "chechnya_myco_lab") %>%
			# find and format all dates
		date_format() %>%
			# detangle dstno
		dstno_detangle() %>%
			# consolidate sample date
		lab_date_consolidator(db = "chechnya_myco_lab") %>%
			# consolidate xpert results
		xpert_result_fixer(set = "chechnya_myco_lab", rm_orig = TRUE) %>%
			# fix lab samples variable
		lab_sample_fixer(set = "chechnya_myco_lab", rm_orig = TRUE) %>%
			# consolidate smear results
		result_consolidator(set = "chechnya_myco_lab", test = "smear", rm_orig = TRUE) %>%
			# consolidate culture results
		result_consolidator(set = "chechnya_myco_lab", test = "culture", rm_orig = TRUE) %>%
			# consolidate DST results
		dst_consolidator(set = "chechnya_myco_lab", aggregate = TRUE, rm_orig = TRUE) %>%
			# consolidate hain mtbdrplus results
		mtbdrplus_fixer(set = "chechnya_myco_lab")

	# check variable names are all present
		vars <- c("dbno", "dstno", "dob", "samp_date", "sample", "smear", "culture",
					"xpert_res", "xpert_rif", "hain_res", "hain_rif", "hain_inh", 
					"dst_p_rif", "dst_p_inh", "dst_p_sli", "dst_p_fq")
		if (! (all(names(x) %in% vars))) {
			warning("All variables in final data frame are not recognised")
		}

	# reorder variables in data frame
		x <- x[vars]

	}
# =================================================================
if (lab == "nukus_myco_lab") {
	x <- x %>%
			# subset all vars required
		subset_vars(set = "nukus_myco_lab") %>%	
			# find and format all dates
		date_format() %>%
			# detangle idno
		id_detangle(db = "epi_info") %>%
			# consolidate sample date
		lab_date_consolidator(db = "nukus_myco_lab")
	
	}	
# =================================================================

if (lab == "nukus_clin_lab") {
	x <- x %>%
			# subset all vars required
		subset_vars(set = "nukus_clin_lab") %>%
			# id number detangle - use epi_info db arg for APID
		id_detangle(db = "epi_info") %>%
			# find and format dates
		date_format() 

	# check variable names are all present
		vars <- c("district", "id", "ds_dr", "date", "test", "result", "comm")
		
		if (! (all(names(x) %in% vars))) {
			warning("All variables in final data frame are not recognised")
		}

	# reorder variables in data frame
		x <- x[vars]

}
# =================================================================
if (lab == "k6_clin_lab") {
	x <- x %>%
			# subset all vars required
		subset_vars(set = "k6_clin_lab") %>%
			# id number detangle - use k6 db arg for registration number
		id_detangle(db = "k6") %>%			
			# find and format dates
		date_format() %>%
			# convert all zeros to NA in continuous variables
		zero_to_na(set = "k6_clin_lab") %>% 
			# convert from wide to long format and remove all results == NA
		lab_longr(set = "k6_clin_lab")
}
# =================================================================
x
}
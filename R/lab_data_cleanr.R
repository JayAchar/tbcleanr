#' Cleans TB laboratory data sets
#'
#' Take laboratory data set and perform multiple adjustments based on which
#' laboratory data set is being used
#' @param x data frame containing variables
#' @param lab define laboratory dat set being used. Values can be - "chechnya".
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @import magrittr
#' @export
#' @examples
#' \dontrun{
#' lab_data_cleanr(p, lab = "chechnya")
#' }


lab_data_cleanr <- function(x, lab = "chechnya") {
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}
# acceptable values for lab
	l <- c("chechnya")

# check lab arg is within acceptable values
	if (! lab %in% l) {
		stop("Specify lab argument within specified values")
	}

# =======================================================
	# clean and convert data frame

if (lab == "chechnya") {
	x <- x %>%
			# subset all vars required
		subset_vars(set = "chechnya_myco_lab") %>%
			# find and format all dates
		date_format() %>%
			# detangle dstno
		dstno_detangle() %>%
			# consolidate sample date
		chechnya_lab_date_consolidator() %>%
			# consolidate xpert results
		xpert_result_fixer(set = "chechnya_myco_lab", rm_orig = TRUE) %>%
			# fix lab samples variable
		lab_sample_fixer(rm_orig = TRUE) %>%
			# consolidate smear results
		result_consolidator(set = "chechnya_myco_lab", test = "smear", rm_orig = TRUE) %>%
			# consolidate culture results
		result_consolidator(set = "chechnya_myco_lab", test = "culture", rm_orig = TRUE) %>%
			# consolidate DST results
		dst_consolidator(set = "chechnya_myco_lab", aggregate = TRUE, rm_orig = TRUE) %>%
			# consolidate hain mtbdrplus results
		mtbdrplus_fixer(set = "chechnya")

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

x
}
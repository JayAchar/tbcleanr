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
		dst_consolidator(aggregate = FALSE, rm_orig = TRUE, ...) %>%
			# consolidate hain mtbdrplus results
		mtbdrplus_fixer()


x
}

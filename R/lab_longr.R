#' Convert lab data from wide to long
#'
#' In specified data sets, convert laboratory results from wide to long format.  
#' @param x data frame containing variables
#' @param set define variable set to apply. Values can be "k6_clin_lab".  
#' @param add string of any additional variables to include
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom tidyr gather
#' @export
#' @examples
#' \dontrun{
#' lab_longr(z, set = "k6_clin_lab")
#' }


lab_longr <- function(x, set, add = NULL, ...) {	
# acceptable values for "set" arg
	s <- c("k6_clin_lab")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check set is within acceptable values
	if (! set %in% s) {
			set_options <- paste(s, collapse = ", ")
			error_message <- paste("\'set\' arg should be ", set_options, sep = "")
		stop(error_message)	}

# =================================================================
# set specific variables 
		if (set == "k6_clin_lab") {
			vars <- c("hemoglobin", "thrombocyt", "ast", "alt", 
						"creatinine", "glucose", "potassium", 
						"magnesium", "serumalbumin")
		}	

	vars <- c(vars, add)		# add additional requested variables

# =================================================================
# check set variables present
	if (! all(vars %in% names(x))) {
		stop("Laboratory variables not all present in data frame")
	}

# convert result variables to long format
	x <- gather(x, test, result, vars)

# remove all records where result == NA
	x <- subset(x, ! is.na(result))

x
}

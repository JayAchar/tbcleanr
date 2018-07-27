#' Convert lab data from wide to long
#'
#' In specified data sets, convert laboratory results from wide to long format.  
#' @param x data frame containing variables
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",  
#' @param add string of any additional variables to include
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom tidyr gather
#' @importFrom lubridate dmy
#' @export
#' @examples
#' \dontrun{
#' lab_longr(z, set = "k6_clin_lab")
#' }


lab_longr <- function(x, software = c("excel", "koch_6", "epiinfo"),
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

# =================================================================
# set specific variables 
		if (software == "koch_6" && file == "clinical_lab") {
			vars <- c("hemoglobin", "thrombocyt", "ast", "alt", 
						"creatinine", "glucose", "potassium", 
						"magnesium", "serumalbumin")
		}	else {
			return(x)
		}

	vars <- c(vars, add)		# add additional requested variables

# =================================================================
# check set variables present
	if (! all(vars %in% names(x))) {
		stop("Laboratory variables not all present in data frame")
	}

# convert result variables to long format
	x <- gather(x, key = "test", value = "result", vars)

# convert labclindate to date format
	x$labclindate <- dmy(x$labclindate)

# remove all records where result == NA or 0
	x <- subset(x, ! is.na(result))
	x <- subset(x, x$result > 0)

x
}

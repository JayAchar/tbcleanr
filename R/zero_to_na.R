#' Convert zero's to NA in data frame
#'
#' In specified data sets, convert all zero's to NAs in specific variables.  
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
#' @importFrom purrr map_at
#' @export
#' @examples
#' \dontrun{
#' zero_to_na(z, set = "k6_clin_lab")
#' }

zero_to_na <- function(x, software = c("excel", "koch_6", "epiinfo"),
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
		} else {
			return(x)
		}	

	vars <- c(vars, add)		# add additional requested variables

# =================================================================
# check set variables present
	if (! all(vars %in% names(x))) {
		stop("Laboratory variables not all present in data frame")
	}

# convert all zeros to NArm()
	zero <- function(z) {
		z[z == 0] <- NA
		z
	}

# map function to variables
	x[] <- map_at(x, .at = vars, .f = zero)

x
}

#' Convert binary variables to factorised output
#'
#' Take data frame with binary variables and output 
#' binary factorised variables 
#' @param x data frame containing drug variables
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at
#' @export
#' @examples
#' \dontrun{
#' adm_binary_fixer(p, set = "msc500")
#' }

adm_binary_fixer <- function(x, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"), 
								...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)

# =================================================================
# set set specific variables 
		if (software == "koch_6" && file == "adm") {
			v <- c("diabetes", "cardiodi", "renalfail")
		} else if (software == "epiiinfo" && file == "adm") {
			# numerical variables
			v <- c("DIABETES","CARDIODI", "RENALFAI","PSYCHI", "SEIZURE", "HEPADIS",
					"ALCO")
			# character variables
			chr <- c("HD", "EE", "RR", "ZP","CSC", "SMS", "AMA", "KMK", "CPX", "OFX",
					"TT", "ETHE","PASP", "AMXC","CFZ", "CLRC","CMC", "OTH", "EVER",
					"INJECT", "HOMELESS", "HEALTHWO", "PRIWO", "TOBACCO")
		} else {
			return(x)
		}

# =================================================================
# check all numerical variables exist in data frame
	if (! all(v %in% names(x))) {
		warning("All binary variables not included in data frame - check -set- arg")
	}

# recode numerical variables 
	x[] <- map_at(x, .at = v, .f = yn_binary_fixer)


# recode character variables
	if (exists("chr")) {
		x[] <- map_at(x, .at = c, .f = yn_binary_fixer)
	} 
	

x
}

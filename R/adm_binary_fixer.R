#' Convert binary variables to factorised output
#'
#' Take data frame with binary variables and output 
#' binary factorised variables 
#' @param x data frame containing drug variables
#' @param set define variable set to apply. Values can be "msc500",
#' "k6_adm_standard", "nukus_epi_info"
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at
#' @export
#' @examples
#' \dontrun{
#' adm_binary_fixer(p, set = "msc500")
#' }



adm_binary_fixer <- function(x, set, ...) {
# acceptable values for "set" arg
	s <- c("msc500", "k6_adm_standard", "nukus_epi_info")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check set is within acceptable values
	if (! set %in% s) {
		stop("Specify set argument within specified values")
	}
# =================================================================
# set set specific variables 
		if (set %in% c("msc500", "k6_adm_standard")) {
			v <- c("diabetes", "cardiodi", "renalfail")
		}	
		if (set == "nukus_epi_info") {
			# numerical variables
			v <- c("DIABETES","CARDIODI", "RENALFAI","PSYCHI", "SEIZURE", "HEPADIS",
					"ALCO")
			# character variables
			chr <- c("HD", "EE", "RR", "ZP","CSC", "SMS", "AMA", "KMK", "CPX", "OFX",
					"TT", "ETHE","PASP", "AMXC","CFZ", "CLRC","CMC", "OTH", "EVER",
					"INJECT", "HOMELESS", "HEALTHWO", "PRIWO", "TOBACCO")
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
	

return(x)
}

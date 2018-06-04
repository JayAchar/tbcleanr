#' Convert drug doses to binary output
#'
#' Take data frame with drug dosing variables and output 
#' binary factorised variables 
#' @param x data frame containing drug variables
#' @param set define variable set to apply. Values can be "msc500",
#' "k6_adm_standard", "nukus_epi_info"
#' @param ... further arguments passed to or from other methods.
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at
#' @export
#' @examples
#' \dontrun{
#' drug_fixer(p, set = "msc500")
#' }

drug_fixer <- function(x, set, ...) {
# acceptable values for "set" arg
	s <- c("msc500", "k6_adm_standard", "nukus_epi_info")

# check set is within acceptable values
	if (! set %in% s) {
		stop("Specify set argument within specified values")
	}

# check drug variables correct based on set arg
	if (set %in% c("msc500", "k6_adm_standard")) {
		d <- c("E", "H", "R", "Z", "Am", "Cm", "Km", "Lfx", "Mfx", 
			"Ofx", "Cs", "Eto", "PAS", "PAS Na", "Pto", "Amx-Clv",
			"Bdq", "Cfz", "Clr", "Dld", "hdH", "ImpCln", "Lzd", "Mpm")
	}

if (set == "nukus_epi_info") {
		d <- c("HDH","RDR", "EDE", "ZDZ", "SDS",
					"KADKA", "OFLDOFL", "CAPDCAP", "ETHDETH", "CYCLDCYCL", "AMXDAMX", "PASDPAS",
					"CLADCLA", "CLODCLO", "LXDLX", "MXDMX", "PTDPT", "LZDDLZD", "IMPDIMP",
					"BDQDBDQ")
		# convert all variables to numerical for yn_binary_fixer()
		x[] <- map_at(x, .at = d, .f = as.numeric)
	}	

# recode drug variables 
	x[] <- map_at(x, .at = d, .f = yn_binary_fixer)

return(x)
}

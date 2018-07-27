#' Convert drug doses to binary output
#'
#' Take data frame with drug dosing variables and output 
#' binary factorised variables 
#' @param x data frame containing drug variables
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param ... further arguments passed to or from other methods.
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at
#' @export
#' @examples
#' \dontrun{
#' drug_fixer(p, set = "msc500")
#' }

drug_fixer <- function(x, software = c("excel", "koch_6", "epiinfo"),
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

# check drug variables correct based on set arg
	if (software == "koch_6" && file == "adm") {
		d <- c("E", "H", "R", "Z", "Am", "Cm", "Km", "Lfx", "Mfx", 
			"Ofx", "Cs", "Eto", "PAS", "PAS Na", "Pto", "Amx-Clv",
			"Bdq", "Cfz", "Clr", "Dld", "hdH", "ImpCln", "Lzd", "Mpm")		
	} else if (software == "epiinfo" && file == "adm") {
		d <- c("HDH","RDR", "EDE", "ZDZ", "SDS",
			"KADKA", "OFLDOFL", "CAPDCAP", "ETHDETH", "CYCLDCYCL", "AMXDAMX", "PASDPAS",
			"CLADCLA", "CLODCLO", "LXDLX", "MXDMX", "PTDPT", "LZDDLZD", "IMPDIMP",
			"BDQDBDQ")
		# convert all variables to numerical for yn_binary_fixer()
		x[] <- map_at(x, .at = d, .f = as.numeric)
	} else {
		return(x)
	}

# recode drug variables 
	x[] <- map_at(x, .at = d, .f = yn_binary_fixer)

x
}

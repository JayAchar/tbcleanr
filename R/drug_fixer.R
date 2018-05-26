#' Convert drug doses to binary output
#'
#' Take data frame with drug dosing variables and output 
#' binary factorised variables 
#' @param x data frame containing drug variables
#' @param set define variable set to apply. Values can be "msc500",
#' "k6_adm_standard"
#' @param ... further arguments passed to or from other methods.
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' drug_fixer(p, set = "msc500")
#' }

drug_fixer <- function(x, set = "msc500", ...) {

# check drug variables correct based on set arg
	if (set %in% c("msc500", "k6_adm_standard")) {
		d <- c("E", "H", "R", "Z", "Am", "Cm", "Km", "Lfx", "Mfx", 
			"Ofx", "Cs", "Eto", "PAS", "PAS Na", "Pto", "Amx-Clv",
			"Bdq", "Cfz", "Clr", "Dld", "hdH", "ImpCln", "Lzd", "Mpm")
	}


if (! set == "") {
	if (! all(d %in% names(x))) {
		warning("All drug variables not included in data frame - check -set- arg")
	}
}

# recode drug variables 
	x[] <- map_at(x, .at = d, .f = yn_binary_fixer)

return(x)
}

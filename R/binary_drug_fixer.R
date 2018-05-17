#' Convert drug drug doses to binary output
#'
#' Take data frame with drug dosing variables and output 
#' binary factorised variables 
#' @param x data frame containing drug variables
#' @param set define variable set to apply. Values can be "msc500"
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' binary_drug_fixer(p, set = "msc500")
#' }

binary_drug_fixer <- function(x, set = "msc500") {

# check drug variables correct based on set arg
	if (set == "msc500") {
		d <- c("E", "H", "R", "Z", "Am", "Cm", "Km", "Lfx", "Mfx", 
			"Ofx", "Cs", "Eto", "PAS", "PAS Na", "Pto", "Amx-Clv",
			"Bdq", "Cfz", "Clr", "Dld", "hdH", "ImpCln", "Lzd", "Mpm")

	}

if (! set == "") {
	if (! all(d %in% names(x))) {
		warning("All drug variables not included in data frame - check -set- arg")
}
}

# loop through all drug variables 
		for (i in d) {
				x[[i]][is.na(x[[i]])] <- 0

				x[[i]][x[i] > 0] <- 1
				
				x[[i]] <- factor(x[[i]], 
							levels = c("0", "1"),
							labels = c("No", "Yes"))
			}

return(x)
}

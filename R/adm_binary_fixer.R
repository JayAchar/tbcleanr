#' Convert binary variables to factorised output
#'
#' Take data frame with binary variables and output 
#' binary factorised variables 
#' @param x data frame containing drug variables
#' @param set define variable set to apply. Values can be "msc500",
#' "k6_adm_standard"
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' adm_binary_fixer(p, set = "msc500")
#' }



adm_binary_fixer <- function(x, set = "msc500") {

# check drug variables correct based on set arg
	if (set %in% c("msc500", "k6_adm_standard")) {
		v <- c("diabetes", "cardiodi", "renalfail")
	}


if (! set == "") {
	if (! all(v %in% names(x))) {
		warning("All binary variables not included in data frame - check -set- arg")
	}
}

# recode drug variables 
	x[] <- map_at(x, .at = v, .f = yn_binary_fixer)

return(x)
}

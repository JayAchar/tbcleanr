#' Subset Koch 6 admission variables
#'
#' Subset Koch 6 admission variables along pre-defined variable  
#' sets   
#' @param x data frame containing Koch 6 admission variables
#' @param set define variable set to apply. Values can be "msc500"
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' k6_adm_subset_vars(p, set = "msc500")
#' }


k6_adm_subset_vars <- function(x, set = "msc500") {
# acceptable values for "set" arg
	s <- c("msc500")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check set is valid
	if (set == "") {
		stop("Specify set argument in function call")
	}

# check set is within acceptable values
	if (! set %in% s) {
		stop("Specify set argument within specified values")
	}

# set = msc500
	if (set == "msc500") {
		k <- c("registrationnb", "dateofbirth", "datedeat", "gender",
					"weight", "height", "ecgqt", "ecghr", "ecgrr", "ecgqtcf",
					"diabetes", "cardiodi", "renalfail", "cav", "cavD", "labClinDate",
					"Hemoglobin", "Creatinine", "HIV", "CD4count", "Starttre",
					"E", "H", "R", "Z", "Am", "Cm", "Km", "Lfx", "Mfx", 
					"Ofx", "Cs", "Eto", "PAS", "PAS Na", "Pto", "Amx-Clv",
					"Bdq", "Cfz", "Clr", "Dld", "hdH", "ImpCln", "Lzd", "Mpm",
					"dateend", "dateout", "outfirst", "outfirst2013", "2013outcome")
		x <- subset(x, select = k)
		return(x)
	}


return(x)	
}
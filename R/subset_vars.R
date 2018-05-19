#' Subset variables
#'
#' Subset variables along pre-defined variable  
#' sets   
#' @param x data frame containing variables
#' @param set define variable set to apply. Values can be "msc500", "chechnya_myco_lab"
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' subset_vars(p, set = "msc500")
#' }


subset_vars <- function(x, set = "msc500") {
# acceptable values for "set" arg
	s <- c("msc500", "chechnya_myco_lab")

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
					"Hemoglobin", "Creatinine", "cdhivenrol", "HIV", "CD4count", "Starttre",
					"E", "H", "R", "Z", "Am", "Cm", "Km", "Lfx", "Mfx", 
					"Ofx", "Cs", "Eto", "PAS", "PAS Na", "Pto", "Amx-Clv",
					"Bdq", "Cfz", "Clr", "Dld", "hdH", "ImpCln", "Lzd", "Mpm",
					"dateend", "dateout", "outfirst", "outfirst2013", "2013outcome")
	}

# set = chechnya_myco_lab
	if (set == "chechnya_myco_lab") {
		k <- c("dbno", "dstno", "dob", "sputum", "dcol1", "dcol2", "dcol3",
					"micro1", "micro2", "micro3", "dcolxpert1", "dcolxpert2", "xpert1err",
					"xpert2err", "xpert1res", "xpert2res", "xpert1rif", "xpert2rif",
					"mgit1d", "mgit2d", "mgit3d", "mgitres",
					"ms", "mr", "mh", "mz", "me", "mcm", "mam", "mlfx", "lj1d", "lj2d",
					"lj3d", "ljres", 
					"ljs", "ljr", "ljh", "ljz", "lje",
					"ctmicres", "ctmgitres", "ctidres",
					"cts", "ctr", "cth", "ctz", "cte", "ctcm", "ctam", "ctlfx", "ctmfx",
					"ctmfx2", "ctlzd", "cthres", "cthrifres", "cthinhres")
	}

		x <- subset(x, select = k)
return(x)	
}
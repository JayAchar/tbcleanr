#' Aggregate dates in lab database
#'
#' Consolidate sample dates in mycobacterial lab database
#' @param x data frame containing sample date variables
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom lubridate is.Date
#' @examples
#' \dontrun{
#' lab_date_consolidator(p, set = "msc500")
#' }


lab_date_consolidator <- function(x, software = c("excel", "koch_6", "epiinfo"),
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
# set db specific variables 
		if (software == "excel" && project == "chechnya" && file == "lab") {
			vars <- c("dcol1", "dcol2", "dcol3")
		}	else if (software %in% c("excel", "epiinfo") && project == "kk" && file == "lab") {
			vars <- c("FIRST", "SECOND", "THIRD")
		} else {
			return(x)
		}
# =================================================================
# check variables are present
	if (! all(vars %in% names(x))) {
		stop("All date variables not available - check set arg")
	}

# check all variables are dates
	if (! all(as.logical(lapply(x[ , vars], is.Date)))) {
		stop("All variables are not classed as dates")
	}

# aggregate sample collection date
	x$samp_date <- pmin(x[[vars[1]]], x[[vars[2]]], x[[vars[3]]], na.rm = T)


# remove old variables
	x[ , vars] <- NULL

x
}

#' Aggregate dates in lab database
#'
#' Consolidate sample dates in mycobacterial lab database
#' @param x data frame containing sample date variables
#' @param db define database being used - "chechnya_myco_lab", "nukus_myco_lab"
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom lubridate is.Date
#' @examples
#' \dontrun{
#' lab_date_consolidator(p, set = "msc500")
#' }


lab_date_consolidator <- function(x, db, ...) {
# acceptable values for db arg
	s <- c("chechnya_myco_lab", "nukus_myco_lab")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check db is within acceptable values
	if (! db %in% s) {
		stop("Specify db argument within specified values")
	}

# =================================================================
# set db specific variables 
		if (db == "chechnya_myco_lab") {
			vars <- c("dcol1", "dcol2", "dcol3")
		}	
		if (db == "nukus_myco_lab") {
			vars <- c("FIRST", "SECOND", "THIRD")
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

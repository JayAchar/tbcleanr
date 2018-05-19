#' Detangle APID identification number into rayon, DS/DR TB and ID number
#'
#' Separate APID numbers into 3 constituent parts to ascertain 
#' district, DS or DR TB categorisation and numerical ID number
#' @param x data frame containing APID variable from KK programme
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @import stringr
#' @examples
#' \dontrun{
#' apid_detangle(p)
#' }

apid_detangle <- function(x) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check variable exists
	if (! any(c("APID", "apid") %in% names(x))) {
		stop("APID variable not present in data frame")
	} 

# convert variable name to lower case
	if (any(names(x) == "APID")) {
		n <- which(names(x) == "APID")
		names(x)[n] <- tolower(names(x)[n])
	}

# warning regarding missing APID in records
	if (sum(is.na(x$apid)) > 0) {
		m <- sum(is.na(x$apid))
		warning(paste("There are", m, "missing APID values in this dataset"))
	}

# if 4th character is "D" = DS TB
	x$ds_dr <- NA		# new empty variable
	ds <- grep("^.{3}D", x$apid)
	x$ds_dr[ds] <- 0
	x$ds_dr[is.na(x$ds_dr)] <- 1
	x$ds_dr <- factor(x$ds_dr, 
										levels = c(0,1),
										labels = c("DS-TB", "DR-TB"))
	x$ds_dr[is.na(x$apid)] <- NA

# take rayon and generate district variable
	x$district <- str_match(x$apid, "[[:alpha:]]{3}")

# take all digits and generate id number
	x$id <- str_match(x$apid, "\\d+")
	x$id <- as.numeric(x$id)


# remove original APID variable
	x$apid <- NULL

return(x)
}
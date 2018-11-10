#' Split admission ID number
#'
#' Handle TB data ID number based on database. If APID, split 
#' into 3 constituent parts to ascertain district, DS or DR TB 
#' categorisation and numerical ID number. If 
#' @param x data frame containing APID variable from KK programme
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom stringr str_match str_extract
#' @importFrom assertthat assert_that

id_detangle.epiinfo <- function(x, ...) {

# check input
	assert_that(is.data.frame(x))

# =================================================================

# check variable exists
	if (! "APID" %in% names(x)) {
		stop("ID variable not present in data frame")
	} 

# warning regarding missing APID in records
	if (sum(is.na(x$APID)) > 0) {
		m <- sum(is.na(x$APID))
		message(paste("There are", m, "missing ID values in the dataset"))
	}

# generate DS/DR variable from APID number

		# if 4th character is "D" = DS TB
			x$ds_dr <- NA		# new empty variable
			ds <- grep("^.{3}D", x$APID)
				x$ds_dr[ds] <- 0
				x$ds_dr[is.na(x$ds_dr)] <- 1
				x$ds_dr <- factor(x$ds_dr, 
							levels = c(0,1),
							labels = c("DS-TB", "DR-TB"))
			x$ds_dr[is.na(x$APID)] <- NA

# take rayon and generate district variable
	x$district <- str_match(x$APID, "[[:alpha:]]{3}") %>% 
	                as.character()

x
}

#' Bentiu Xpert laboratory date adjustment
#'
#' This function takes the Bentiu PoC lab dataset as a data.frame and cleans 
#' the date format and class.
#' @param x data frame including variable called "date"
#' @keywords TB
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export

bentiu_xpert_date <- function(x) {

# check x is data.frame
	if (!is.data.frame(x)) {
		stop("Confirm x is data.frame")
	}

# check date variable present
	if (!"date" %in% names(x)) {
			stop("Confirm date variable present and named date")
	}

# check missing dates have been removed
	if (any(is.na(x$date))) {
		stop("Missing dates detected - must be removed")
	}

# fix dates
		# correctly formatted dates
	x$date1 <- str_extract(x$date, "^\\d{2}/\\d{2}/\\d{2}")
	x$date1 <- dmy(x$date1)

		# dates as numerics which have days and months inverted
	x$date2 <- x$date
	x$date2[str_detect(x$date2, "^\\d{2}/\\d{2}/\\d{2}")] <- NA
	x$date2 <- as.numeric(x$date2)	
	x$date3 <- as.Date(x$date2, origin = "1899-12-30")
	x$date4 <- paste0(year(x$date3), "-", day(x$date3), "-", month(x$date3))
	x$date4 <- ymd(x$date4)

# check no missing dates generated
	if (any(is.na(x$date1) && is.na(x$date4))) {
		stop("Missing test date generated")
	}
# combine two date formats
	x$date1[is.na(x$date1)] <- x$date4[is.na(x$date1)]
	if (any(is.na(x$date1))) {
		stop("Missing test dates generated")
	}

# check no dates in future
	t <- Sys.Date()
	if (any(x$date1 > t)) {
		stop("Future dates generated")
	}

# remove additional variables
	x$date2 <- NULL
	x$date3 <- NULL
	x$date4 <- NULL

# remove original date variable
	x$date <- NULL

# rename date1 to date
	names(x)[names(x) == "date1"] <- "date"

return(x)
}
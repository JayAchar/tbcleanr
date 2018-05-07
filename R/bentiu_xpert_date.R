






bentiu_xpert_date <- function(x) {
		require(stringr)
		require(lubridate)
# check x is data.frame
	if (!is.data.frame(x)) {
		stop("Confirm x is data.frame")
	}

# check date variable present
	if (!"date" %in% names(x)) {
			stop("Confirm date variable present and named date")
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

return(x)
}
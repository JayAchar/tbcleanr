#' Bentiu Xpert laboratory date adjustment
#'
#' This function takes the Bentiu PoC lab dataset as a data.frame and cleans 
#' the date format and class.
#' @param x data frame including variable called "date"
#' @keywords TB
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertive assert_is_data.frame
#' @importFrom stringr str_extract str_detect 
#' @importFrom lubridate dmy mdy
#' @export

oca_xpert_date <- function(x) {

# check x is data.frame
  assert_is_data.frame(x)

# check date variable present
	if (!"date" %in% names(x)) {
			stop("Confirm date variable present and named date")
	}

# check missing dates have been removed
	if (any(is.na(x$date))) {
		stop("Missing dates detected - must be removed")
	}

# =================================== #
# ========  fix dates  =========================== #
# remove times from date variable
	x$date <- str_extract(x$date, "^\\d{2}/\\d{2}/\\d+")

# mdy formatted dates all have 4 digit year
	# detect all dates with 4 digit year and extract
		mdy_date <- str_detect(x$date, pattern = "^\\d{2}/\\d{2}/\\d{4}")
		x$date_mdy <- x$date

	# all non-mdy dates converted to blank
		x$date_mdy[! mdy_date] <- ""

	# convert mdy to date
		x$date_mdy <- mdy(x$date_mdy)

	# extract and convert dmy date
		x$date_dmy <- x$date
		x$date_dmy[mdy_date] <- ""
		x$date_dmy <- dmy(x$date_dmy)

# merge formated dates into new date variable
		x$date_new <- ifelse(is.na(x$date_dmy), x$date_mdy, x$date_dmy)
		x$date_new <- as.Date(x$date_new, origin = "1970-01-01")

# =================================== #
# check if date is extrated into both dmy and mdy
		test <- subset(x, select = c("date_mdy", "date_dmy"))
		test$count <- rowSums(is.na(test))
		if (! all(test$count == 1)) {
			stop("Error in matching dates - some dates unmatched or matched > once.")
		}

# =================================== #
# remove extra variables
		x$date_dmy <- NULL
		x$date_mdy <- NULL
		x$date <- NULL

# rename date1 to date
	names(x)[names(x) == "date_new"] <- "date"

x
}
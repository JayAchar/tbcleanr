#' Detect and convert date variables
#'
#' This function detects variables with dates xx/xx/xxxx and converts them into date objects
#' @param x data frame or data.table
#' @param format specify the lubridate output format
#' @keywords TB
#' @export

date.format <- function(x, format = dmy) {
		require(dplyr)
		require(stringr)
		require(lubridate)
			output <- logical()
			
	for (i in 1:length(names(x)))	{
		x[[i]] %>%
			str_detect("^\\d{2}/\\d{2}/\\d{4}$") %>%
			# omit missing values
			na.omit() %>%
			mean() %>%
			# only keep variables where all records are dates
			as.logical -> output[i]	
	}
	
	# convert variables with all missing to FALSE
		output[is.na(output)] <- FALSE
	
	# format date
		x[output] <- map(x[output], format)

	return(x)
}
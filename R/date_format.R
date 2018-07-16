#' Detect and convert date variables
#'
#' This function detects variables with dates xx/xx/xxxx and converts them into date objects
#' @param x data frame or data.table
#' @param format specify the lubridate output format
#' @param ... further arguments passed to or from other methods
#' @keywords TB
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @importFrom stats na.omit
#' @importFrom lubridate dmy mdy ymd
#' @export

date_format <- function(x, format = dmy, ...) {
	
			output <- logical()

			# strings to detect
			strings <- c("^\\d{2}/\\d{2}/\\d{4}$",
						"^\\d{2} \\w* \\d{4}$",
						"^\\d{2}-\\w*-\\d{2}$")
			
	for (i in 1:length(names(x)))	{
		x[[i]] %>%
			str_detect(paste(strings, collapse = '|')) %>%
			# omit missing values
			na.omit() %>%
			mean() %>%
			# only keep variables where all records are datesa
			as.logical -> output[i]	
	}
	
	# convert variables with all missing to FALSE
		output[is.na(output)] <- FALSE
	
	# format date
		x[output] <- map(x[output], format)

	return(x)
}

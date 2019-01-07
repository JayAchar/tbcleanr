#' Detect and convert date variables
#'
#' This function detects variables with dates xx/xx/xxxx and converts them into date objects
#' @param x data frame or data.table
#' @param ... further arguments passed to or from other methods
#' @keywords TB
#' @importFrom purrr map compose partial map_lgl map_if
#' @importFrom stringr str_detect
#' @importFrom stats na.omit
#' @importFrom lubridate date dmy
#' @importFrom tidyr replace_na
#' @export

date_format <- function(x, ...) {

			# strings to detect
			strings <- c("^\\d{2}/\\d{2}/\\d{4}$",
						"^\\d{2} \\w* \\d{4}$",
						"^\\d{2}-\\w*-\\d{2}$",
						"^\\d{2}-\\d{2}-\\d{4}$")
			
			string_pattern <- paste(strings, collapse = "|")

			# build function for checking for dd/mm/yy character string dates
			chr_date_check <- compose(as.logical,
			                          mean,
			                          na.omit,
			                          partial(str_detect, pattern = string_pattern))
			
			# logical to find all character string dates with pattern
			vec <- map_lgl(x, chr_date_check) %>% 
			  tidyr::replace_na(replace = FALSE)

			# convert all identified character string dates to dmy
			x[] <- map_if(x, vec, dmy) 

  # check for presence of POSIX date variables
			vec_pos <- map_lgl(x, ~class(.x)[1] == "POSIXct")
	
	# convert all POSIX dates
			x[] <- map_if(x, vec_pos, date)
			
	x
}

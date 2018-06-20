#' Consolidate smear results
#'
#' Take laboratory data set and consolidate repeated results to give summary variable
#' @param x data frame containing variables
#' @param set define variable set to apply. Values can be - "chechnya_myco_lab"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param test smear or culture
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom purrr map_at
#' @examples
#' \dontrun{
#' result_consolidator(p, set = "chechnya_myco_lab", rm_orig = TRUE)
#' }


result_consolidator <- function(x, set = "chechnya_myco_lab", test = "smear", rm_orig = TRUE, ...) {

# acceptable values for "set" arg
	s <- c("chechnya_myco_lab")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check set is within acceptable values
	if (! set %in% s) {
			set_options <- paste(s, collapse = ", ")
			error_message <- paste("\'set\' arg should be ", set_options, sep = "")
		stop(error_message)
	}

# smear recoding
	if (test == "smear") {

		# define smear variables
		if (set == "chechnya_myco_lab") {
			smear_vars <- c("micro1", "micro2", "micro3", "ctmicres")
					}

		# use smear_recode function
				x[] <- map_at(x, .at = smear_vars, .f = smear_recode)

		# find maximum smear for each sample
			x$smear <- do.call(pmax, c(x[ , smear_vars], na.rm = T))

		# factorise and order smear variable
		 	x$smear <- factor(x$smear, levels = 0:3,
		 		labels = c("Negative", "1+", "2+", "3+"),
				ordered = T)

		# remove original variables
		 	if (rm_orig %in% c("TRUE", "T")) {
		 		x[, smear_vars] <- NULL
		 	}
	}

# culture recoding
	if (test == "culture") {
		
		# define culture variables
		if (set == "chechnya_myco_lab") {
			culture_vars <- c("mgitres", "ljres", "ctmgitres")
					}	

		# use smear_recode function
				x[] <- map_at(x, .at = culture_vars, .f = culture_recode)

		# find maximum culture for each sample
			x$culture <- do.call(pmax, c(x[ , culture_vars], na.rm = T))

		# factorise and order culture variable
		 	x$culture <- factor(x$culture, levels = 0:1,
		 		labels = c("Negative", "Positive"),
				ordered = T)

		# remove original variables
		 	if (rm_orig %in% c("TRUE", "T")) {
		 		x[, culture_vars] <- NULL
		 	}

	}
return(x)	
}
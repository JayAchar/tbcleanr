#' Consolidate smear results
#'
#' Take laboratory data set and consolidate repeated results to give summary variable
#' @param x data frame containing variables
#' @param set define variable set to apply. Values can be - "chechnya_myco_lab"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' result_consolidator(p, set = "chechnya_myco_lab", rm_orig = TRUE)
#' }


result_consolidator <- function(x, set = "chechnya_myco_lab", rm_orig = TRUE) {

# acceptable values for "set" arg
	s <- c("chechnya_myco_lab")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check set is valid
	if (set == "") {
		stop("Specify set argument in function call")
	}

# check set is within acceptable values
	if (! set %in% s) {
		stop("Specify set argument within specified values")
	}

# recode and as.numeric for smear variables
	smear_vars <- c("micro1", "micro2", "micro3")

		# function to adjust smear variable
			sm_recode <- function(sm) {
				# check var is.character
				if (! is.character(sm)) {
					stop("Smear variable should be is.character")
				}
				# check number of levels of smear variable
				if (length(table(sm)) > 6) {
					stop("Check the number of smear variable levels")
				}

			# recode smear variable
				sm[sm %in% c("Not_done", "Not_Done")] <- NA
				sm[sm == "Negative"] <- 0
				sm[sm == "Scanty"] <- 1
				sm[sm == "1+"]	   <- 1
				sm[sm == "2+"]	   <- 2
				sm[sm == "3+"]     <- 3

			sm <- as.numeric(sm)
			return(sm)
			}

		x[] <- map_at(x, .at = smear_vars, .f = sm_recode)

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

return(x)	
}
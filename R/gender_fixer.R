#' Factorise gender variable
#'
#' Take gender variable from data frame and factorise. Use software
#' to define which data base is being used as the input
#' @param x data frame containing Koch 6 admission variables
#' @param software define database being used - "koch_6", "epiinfo"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertive assert_is_data.frame
#' @examples
#' \dontrun{
#' gender_fixer(p, software = "koch_6")
#' }


gender_fixer <- function(x, software = c("excel", "koch_6", "epiinfo"),
							rm_orig = TRUE, ...) {

# check input
assert_is_data.frame(x)

# check all args
	software <- match.arg(software)

# =================================================================
# set software specific variables 
		if (software == "koch_6") {
			gen_var <- "gender"
		}	
		if (software == "epiinfo") {
			gen_var <- "SEX"
		}
		if (software == "excel") {
			return(x)
		}
# =================================================================

# check gender variable present
	if (! gen_var %in% names(x)) {
		stop("Gender variable not present in data frame")
	}

if (software == "epiinfo") {
# convert character to numeric
	x[[gen_var]][x[[gen_var]] == "M"] <- 1
	x[[gen_var]][x[[gen_var]] == "F"] <- 2
}

if (software == "koch_6") {
	# convert gender == not done to NA
	x[[gen_var]][x[[gen_var]] == 0] <- NA
}

# check levels of gender variable
	if (! length(table(x[[gen_var]])) == 2) {
		stop("Gender variable does not have 2 levels")
	}

# factorise gender variable
	x[[gen_var]] <- factor(x[[gen_var]], levels = c(1:2),
				labels = c("Male", "Female"))

# standardise nameing of gender variable
	if (software == "epiinfo") {
		x$gender <- x[[gen_var]]
	}

# remove original variables
 	if (rm_orig %in% c("TRUE", "T") & software == "epiinfo") {
 		x[, gen_var] <- NULL
 	}

x
}
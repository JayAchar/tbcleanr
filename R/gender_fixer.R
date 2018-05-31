#' Factorise gender variable
#'
#' Take gender variable from data frame and factorise. Use db
#' to define which data base is being used as the input
#' @param x data frame containing Koch 6 admission variables
#' @param db define database being used - "k6", "epi_info"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' gender_fixer(p, db = "k6")
#' }


gender_fixer <- function(x, db = "k6", rm_orig = TRUE, ...) {

# acceptable values for "set" arg
	s <- c("k6", "epi_info")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check db is within acceptable values
	if (! db %in% s) {
		stop("Specify db argument within specified values")
	}

# =================================================================
# set db specific variables 
		if (db == "k6") {
			gen_var <- "gender"
		}	
		if (db == "epi_info") {
			gen_var <- "SEX"
		}
# =================================================================

# check gender variable present
	if (! gen_var %in% names(x)) {
		stop("Gender variable not present in data frame")
	}


# convert gender == not done to NA
	x[[gen_var]][x[[gen_var]] == 0] <- NA

# convert character to numeric
	x[[gen_var]][x[[gen_var]] == "M"] <- 1
	x[[gen_var]][x[[gen_var]] == "F"] <- 2

# check levels of gender variable
	if (! length(table(x[[gen_var]])) == 2) {
		stop("Gender variable does not have 2 levels")
	}

# factorise gender variable
	x[[gen_var]] <- factor(x[[gen_var]], levels = c(1:2),
				labels = c("Male", "Female"))

# standardise nameing of gender variable
	if (db == "epi_info") {
		x$gender <- x[[gen_var]]
	}

# remove original variables
 	if (rm_orig %in% c("TRUE", "T")) {
 		x[, gen_var] <- NULL
 	}

return(x)
}
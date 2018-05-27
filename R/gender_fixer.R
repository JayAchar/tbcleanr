#' Factorise gender variable
#'
#' Take gender variable from data frame and factorise. Use db
#' to define which data base is being used as the input
#' @param x data frame containing Koch 6 admission variables
#' @param db define database being used - "k6"
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' gender_fixer(p, db = "k6")
#' }


gender_fixer <- function(x, db = "k6", ...) {

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

# set gender variable name
		if (db == "k6") {
			gen_var <- "gender"
		}	
		if (db == "epi_info") {
			gen_var <- "SEX"
		}

# check gender variable present
	if (! gen_var %in% names(x)) {
		stop("Gender variable not present in data frame")
	}


# convert gender == not done to NA
	x[[gen_var]][x[[gen_var]] == 0] <- NA

# check levels of gender variable
	if (! length(table(x$gender)) == 2) {
		stop("Gender variable does not have 2 levels")
	}

# factorise gender variable
	x$gender <- factor(x$gender, levels = c(1:2),
				labels = c("Male", "Female"))
}

return(x)
}
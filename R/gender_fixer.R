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

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

	if (db == "") {
		stop("Arg db is missing - please add")
	}



if (db == "k6") {
# check gender variable present
	if (! "gender" %in% names(x)) {
		stop("Gender variable not present in data frame")
	}

# convert gender == not done to NA
	x$gender[x$gender == 0] <- NA

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
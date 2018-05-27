#' Factorise HIV variable
#'
#' Take HIV variables from data frame and factorise. Use db
#' to define which data base is being used as the input
#' @param x data frame containing Koch 6 admission variables
#' @param db define database being used - "k6"
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' hiv_fixer(p, db = "k6")
#' }

hiv_fixer <- function(x, db = "k6", ...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

	if (db == "") {
		stop("Arg db is missing - please add")
	}


if (db == "k6") {
# check variables are present
	h_names <- c("HIV", "cdhivenrol")
	if (! all(h_names %in% names(x))) {
		stop("Required HIV variables not included in data frame")
	}

# convert HIV == not done to NA
	x$HIV[x$HIV %in% c(0, 3, 4)] <- NA
	x$cdhivenrol[x$cdhivenrol %in% c(0, 3:5)] <- NA

# check levels of HIV variable
	if (! length(table(x$HIV)) == 2) {
		stop("HIV variable does not have 2 levels")
	}

	if (! length(table(x$HIV)) == 2) {
		stop("History of HIV variable does not have 2 levels")
	}

# generate aggregate HIV variable
	x$hiv <- NA

	# all those with a result start as negative
	x$hiv[! (is.na(x$HIV) & is.na(x$cdhivenrol))] <- 0
	x$hiv[x$HIV == 1 | x$cdhivenrol == 1] <- 1


# factorise gender variable
	x$hiv <- factor(x$hiv, levels = c(0:1),
				labels = c("Negative", "Positive"))

# remove original, unused HIV variables
	x$HIV <- NULL
	x$cdhivenrol <- NULL

}


return(x)
}
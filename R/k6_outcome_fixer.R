#' Fix Koch 6 treatment outcomes
#'
#' Combine Koch 6 treatment outcome variables to leave one 
#' factorised, labelled variable
#' @param x data frame containing Koch 6 outcome variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' k6_outcome_fixer(p)
#' }

k6_outcome_fixer <- function(x, ...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# outcome variables in Koch 6
	v <- c("outfirst", "outfirst2013", "2013outcome")

	if (! all(v %in% names(x))) {
		stop("Check all outcome variables are included in data frame")
	}

# change format of variable names to comply with R custom
	x$choice <- x[["2013outcome"]]
	x[["2013outcome"]] <- NULL

# generate final outcome variable
	x$outcome <- NA	

# use all correct results from "outfirst" variable
	x$outcome[is.na(x$choice)] <- x$outfirst[is.na(x$choice)]

# use all correct results from "outfirst2013" variable
	w <- which(x$choice == "est 1 outcome 2013")
	x$outcome[w] <- x$outfirst2013[w]

# remove excess variables
	x$outfirst <- NULL
	x$outfirst2013 <- NULL
	x$choice <- NULL

# factorise outcome variable
	x$outcome <- factor(x$outcome, 
				levels = c(0:8),
				labels = c("On treatment",
						"Cured",
						"Completed",
						"Death",
						"Fail",
						"LTFU",
						"Transfer out",
						"Other",
						"Transfer back to SCC"))

# final checks
	if (any(is.na(x$outcome))) {
		warning("Missing outcomes have been introduced")
	}	

return(x)
}
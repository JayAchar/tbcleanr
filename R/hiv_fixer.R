#' Factorise HIV variable
#'
#' Take HIV variables from data frame and factorise. Use software
#' to define which data base is being used as the input
#' @param x data frame containing Koch 6 admission variables
#' @param software define database being used - "koch_6", "epiinfo"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' hiv_fixer(p, software = "koch_6")
#' }

hiv_fixer <- function(x, software = c("excel", "koch_6", "epiinfo"),
						rm_orig = TRUE, ...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
	software <- match.arg(software)

# =================================================================
# set software specific variables 
		if (software == "koch_6") {
			h_names <- c("HIV", "cdhivenrol")
			hiv <- "HIV"
			cdhiv <- "cdhivenrol"
		}	
		if (software == "epiinfo") {
			h_names <- "HIV"
			hiv <- "HIV"
		}
		if (software == "excel") {
			return(x)
		}
# =================================================================

# check variables are present
			if (! all(h_names %in% names(x))) {
				stop("Required HIV variables not included in data frame")
			}

# generate aggregate HIV variable
			x$hiv <- NA

# if software = "koch_6"
		if (software == "koch_6") {
		# all those with a result start as negative
			x$hiv[x[[hiv]] %in% c(1, 2) | x[[cdhiv]] %in% c(1,2)] <- 0
			x$hiv[x[[hiv]] == 1 | x[[cdhiv]] == 1] <- 1
		}

# if software = epiinfo
		if (software == "epiinfo") {
	
			# all those with a result start as negative
			x$hiv[x[[hiv]] == 2] <- 0
			x$hiv[x[[hiv]] == 1] <- 1

		}

# check levels of original HIV variable
			if (! length(table(x$hiv)) <= 2) {
				stop("HIV variable has greater than 2 levels")
			}

# factorise gender variable
	x$hiv <- factor(x$hiv, levels = c(0:1),
				labels = c("Negative", "Positive"))

# remove original variables
		 	if (rm_orig %in% c("TRUE", "T")) {
		 		x[, h_names] <- NULL
		 	}

x
}

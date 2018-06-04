#' Factorise HIV variable
#'
#' Take HIV variables from data frame and factorise. Use db
#' to define which data base is being used as the input
#' @param x data frame containing Koch 6 admission variables
#' @param db define database being used - "k6", "epi_info"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' hiv_fixer(p, db = "k6")
#' }

hiv_fixer <- function(x, db = "k6", rm_orig = TRUE, ...) {

# acceptable values for "set" arg
	s <- c("k6", "epi_info")

# check db is within acceptable values
	if (! db %in% s) {
		stop("Specify db argument within specified values")
	}

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}


# =================================================================
# set db specific variables 
		if (db == "k6") {
			h_names <- c("HIV", "cdhivenrol")
			hiv <- "HIV"
			cdhiv <- "cdhivenrol"
		}	
		if (db == "epi_info") {
			h_names <- "HIV"
			hiv <- "HIV"
		}
# =================================================================

# check variables are present
			if (! all(h_names %in% names(x))) {
				stop("Required HIV variables not included in data frame")
			}

# generate aggregate HIV variable
			x$hiv <- NA

# if db = "k6"
		if (db == "k6") {
		# all those with a result start as negative
			x$hiv[x[[hiv]] %in% c(1, 2) | x[[cdhiv]] %in% c(1,2)] <- 0
			x$hiv[x[[hiv]] == 1 | x[[cdhiv]] == 1] <- 1
		}

# if db = epi_info
		if (db == "epi_info") {
	
			# all those with a result start as negative
			x$hiv[x[[hiv]] == 2] <- 0
			x$hiv[x[[hiv]] == 1] <- 1

		}

# check levels of original HIV variable
			if (! length(table(x$hiv)) == 2) {
				stop("HIV variable does not have 2 levels")
			}

# factorise gender variable
	x$hiv <- factor(x$hiv, levels = c(0:1),
				labels = c("Negative", "Positive"))

# remove original variables
		 	if (rm_orig %in% c("TRUE", "T")) {
		 		x[, h_names] <- NULL
		 	}

return(x)
}

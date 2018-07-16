#' Fix Koch 6 treatment outcomes
#'
#' Combine  treatment outcome variables to leave one 
#' factorised, labelled variable
#' @param x data frame containing outcome variables
#' @param software define database being used - "koch_6", "epiinfo"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' outcome_fixer(p, software = "epiinfo")
#' }

outcome_fixer <- function(x, software = c("excel", "koch_6", "epiinfo"),
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
			# outcome variables in Koch 6
				v <- c("outfirst", "outfirst2013", "2013outcome")
				v1 <- "outfirst"
				v2 <- "outfirst2013"
				v3 <- "2013outcome"
				labs <- c("On treatment",
								"Cured",
								"Completed",
								"Death",
								"Fail",
								"LTFU",
								"Transfer out",
								"Other",
								"Transfer back to SCC")
		}	
		if (software == "epiinfo") {
			v <- "RES"
			labs <- c("On treatment",
								"Cured",
								"Completed",
								"Death",
								"Fail",
								"LTFU",
								"Transfer out",
								"Fail & amplify",
								"Transfer to Cat 4")
		}
		if (software == "excel") {
			return(x)
		}
# =================================================================
# check outcome variables present
	if (! all(v %in% names(x))) {
		stop("Check all outcome variables are included in data frame")
	}

# generate final outcome variable
	x$outcome <- NA	

if (software == "koch_6") {
		# use all correct results from "outfirst" variable
			x$outcome[is.na(x[[v3]])] <- x[[v1]][is.na(x[[v3]])]

		# use all correct results from "outfirst2013" variable
			w <- which(x[[v3]] == "est 1 outcome 2013")
			x$outcome[w] <- x[[v2]][w]	

		}

if (software == "epiinfo") {
	# duplicate outcome variable
		x$outcome <- x[[v]]
}


# factorise outcome variable
			x$outcome <- factor(x$outcome, 
						levels = c(0:8),
						labels = labs)


# remove original variables
 	if (rm_orig %in% c("TRUE", "T")) {
 		x[, v] <- NULL
 	}

x
}

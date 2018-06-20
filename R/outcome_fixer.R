#' Fix Koch 6 treatment outcomes
#'
#' Combine  treatment outcome variables to leave one 
#' factorised, labelled variable
#' @param x data frame containing outcome variables
#' @param db define database being used - "k6", "epi_info"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' outcome_fixer(p, db = "epi_info")
#' }

outcome_fixer <- function(x, db = "k6", rm_orig = TRUE, ...) {

# acceptable values for "set" arg
	s <- c("k6", "epi_info")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check db is within acceptable values
	if (! db %in% s) {
			set_options <- paste(s, collapse = ", ")
			error_message <- paste("\'db\' arg should be ", set_options, sep = "")
		stop(error_message)	
	}

# =================================================================
# set db specific variables 
		if (db == "k6") {
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
		if (db == "epi_info") {
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
# =================================================================
# check outcome variables present
	if (! all(v %in% names(x))) {
		stop("Check all outcome variables are included in data frame")
	}

# generate final outcome variable
	x$outcome <- NA	

if (db == "k6") {
		# use all correct results from "outfirst" variable
			x$outcome[is.na(x[[v3]])] <- x[[v1]][is.na(x[[v3]])]

		# use all correct results from "outfirst2013" variable
			w <- which(x[[v3]] == "est 1 outcome 2013")
			x$outcome[w] <- x[[v2]][w]	

		}

if (db == "epi_info") {
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

return(x)
}

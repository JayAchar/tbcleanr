#' Fix and aggregate Koch 6 x-ray cavities variables
#'
#' Input data frame containing cavity variables, factorise 
#' and aggregate into new variable -cavity-
#' @param x data frame containing xray variable data
#' @param db define database being used - "k6", "epi_info"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' cavities_fixer(p, db = "epi_info", rm_orig = TRUE)
#' }


cavities_fixer <- function(x, db = "k6", rm_orig = TRUE, ...) {
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
			v1 <- "cav"
			v2 <- "cavD"
			v <- c(v1, v2)	
		}	
		if (db == "epi_info") {
			v1 <- "XRAYRES"
			v2 <- "ABNORM"
			v <- c(v1, v2)
		}
# =================================================================

	if (! all(v %in% names(x))) {
		warning("Check all x-ray variables present in data frame")
		return(x)
	}

# generate aggregate binary variable
	x$cavity <- NA

if (db == "k6") {
	# check levels of x-ray variables
	if (! length(table(x[[v1]])) && length(table(x[[v2]])) == 3) {
		stop("X-ray variables have incorrect number of levels")
	}
	# factorise x-ray variables
		factorise_xray <- function(x) {
			x <- factor(x, levels = c(0:2),
					labels = c("None",
								"1 lobe",
								"> 1 lobe"))
			return(x)
		}

	# apply factorise function to drug variables
		x[[v1]]  <- factorise_xray(x[[v1]])
		x[[v2]] <- factorise_xray(x[[v2]])

		# no cavities
	x$cavity[x[[v1]] == "None" && x[[v2]] == "None"] <- 0
		# cavities present
	x$cavity[is.na(x$cavity)] <- 1
}

if (db == "epi_info") {
	# check levels of x-ray variables
	if (! length(table(x[[v1]])) && length(table(x[[v2]])) < 5) {
		stop("X-ray variables have incorrect number of levels")
	}

	# Xray normal
	x$cavity[x[[v1]] == 1] <- 0
	# Xray not done
	x$cavity[x[[v1]] %in% c(3, 4)] <- NA
	# abnormal xray with cavity
	x$cavity[x[[v1]] == 2 & x[[v2]] == 1] <- 1
	# abnormal xray with no cavity
	x$cavity[x[[v1]] == 2 & x[[v2]] == 2] <- 0	

	# factorise original variables
	x[[v1]] <- factor(x[[v1]], levels = c(1:4),
					labels = c("Normal",
								"Abnormal",
								"Not done",
								"Unknown"))
	x[[v2]] <- factor(x[[v2]], levels = c(1:2), 
					labels = c("Cavitary",
								"Non-cavitary"))
}

		# factorise new variable
	x$cavity <- factor(x$cavity, levels = c(0:1),
					labels = c("No", "Yes")) 
 
# remove original variables
 	if (rm_orig %in% c("TRUE", "T")) {
 		x[, v] <- NULL
 	}
 	

return(x)
}

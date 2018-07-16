#' Fix and aggregate Koch 6 x-ray cavities variables
#'
#' Input data frame containing cavity variables, factorise 
#' and aggregate into new variable -cavity-
#' @param x data frame containing xray variable data
#' @param software define database being used - "koch_6", "epiinfo"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' cavities_fixer(p, software = "epiinfo", rm_orig = TRUE)
#' }


cavities_fixer <- function(x, software = c("excel", "koch_6", "epiinfo"),
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
			v1 <- "cav"
			v2 <- "cavD"
			v <- c(v1, v2)	
		}	
		if (software == "epiinfo") {
			v1 <- "XRAYRES"
			v2 <- "ABNORM"
			v <- c(v1, v2)
		}
		if (software == "excel") {
			return(x)
		}
# =================================================================

	if (! all(v %in% names(x))) {
		warning("Check all x-ray variables present in data frame")
		return(x)
	}

# generate aggregate binary variable
	x$cavity <- NA

if (software == "koch_6") {
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

if (software == "epiinfo") {
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
 	

x
}

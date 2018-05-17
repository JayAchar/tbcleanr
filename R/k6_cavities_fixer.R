#' Fix and aggregate Koch 6 x-ray cavities variables
#'
#' Input data frame containing cavity variables, factorise 
#' and aggregate into new variable -cavity-
#' @param x data frame containing Koch 6 HBV and HCV variables
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' k6_cavities_fixer(p)
#' }


k6_cavities_fixer <- function(x) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}	

# x-ray variables in Koch 6
	v <- c("cav", "cavD")

	if (! all(v %in% names(x))) {
		warning("Check all x-ray variables present in data frame")
		return(x)
	}

# check levels of x-ray variables
	if (! length(table(x$cav)) && length(table(x$cavD)) == 3) {
		stop("X-ray variables have incorrect number of levels")
	}

# factorise x-ray variables
	factorise_drugs <- function(x) {
		x <- factor(x, levels = c(0:2),
				labels = c("None",
							"1 lobe",
							"> 1 lobe"))
		return(x)
	}

# apply factorise function to drug variables
	x$cav  <- factorise_drugs(x$cav)
	x$cavD <- factorise_drugs(x$cavD)

# generate aggregate binary variable
	x$cavity <- NA
		# no cavities
	x$cavity[x$cav == "None" && x$cavD == "None"] <- 0
		# cavities present
	x$cavity[is.na(x$cavity)] <- 1

		# factorise new variable
	x$cavity <- factor(x$cavity, levels = c(1:2),
					labels = c("No", "Yes")) 
 	

return(x)
}

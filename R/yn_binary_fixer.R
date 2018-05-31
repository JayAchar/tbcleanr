#' Factorise binary (Y/N) variables
#'
#' Convert numerical variable into factor Y/N
#' @param x numerical variable with 2 levels - 0, 1
#' @param class specify class of variables to be parsed - "num", "chr"
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' yn_binary_fixer(p$renalfail, class = "num")
#' }


yn_binary_fixer <- function(x, class, ...) {
# acceptable values for "set" arg
	s <- c("num", "chr")

# check class is within acceptable values
	if (! class %in% s) {
		stop("Specify class argument within specified values")
	}

# remove variables with all NAs
	if (all(is.na(x))) {
		
		message("Y/N variable dropped as all NAs")
		x <- NULL

	} else {

		if (class == "num") {
			
			# check variable class
			if (! is.numeric(x)) {
				stop("Y/N variable not classed as numeric")
			}
			
			# check levels - confirm Y/N status
			if (! length(table(x)) == 2) {
				stop("Number of levels of Y/N variable is not 2")
			}
			
		# convert to binary variable
			x[x > 0] <- 1

		}


		if (class == "chr") {
			
			# check variable class
			if (! is.character(x)) {
				stop("Y/N variable not classed as charactre")
			}

			# check levels - confirm Y/N status
			if (! length(table(x)) == 2) {
				stop("Number of levels of Y/N variable is not 2")
			}
			# convert to numerical variable
			x[x == "N"] <- 0
			x[x == "Y"] <- 1
			x <- as.numeric(x)
		}
		
		# factorise
			x <- factor(x, levels = c(0,1),
						labels = c("No", "Yes"))

	}


return(x)
}

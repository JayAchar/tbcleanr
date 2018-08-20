#' Factorise binary (Y/N) variables
#'
#' Convert numerical variable into factor Y/N
#' @param x numerical variable with 2 levels - 0, 1
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' yn_binary_fixer(p$renalfail, class = "num")
#' }


yn_binary_fixer <- function(x, ...) {
# acceptable values for "set" arg
	s <- c("integer", "numeric", "character")

# record class of variable
	class <- class(x)

# check class is within acceptable values
	if (! class %in% s) {
			set_options <- paste(s, collapse = ", ")
			error_message <- paste("\'class\' of variable should be one of the following: ",
															 set_options, sep = "")
		stop(error_message)
	}


		if (class %in% s[1:2]) {
			
			# check variable class
			if (! is.numeric(x)) {
				stop("Y/N variable not classed as numeric")
			}
			
		# convert to binary variable
			x[x > 0] <- 1

		}

		if (class == s[3]) {
			
			# check variable class
			if (! is.character(x)) {
				stop("Y/N variable not classed as character")
			}

			# convert to numerical variable
			x[! (x %in% c("N", "Y"))] <- NA
			x[x == "N"] <- 0
			x[x == "Y"] <- 1

			x <- as.numeric(x)
		}
		
		# factorise
			x <- factor(x, levels = c(0,1),
						labels = c("No", "Yes"))



x
}

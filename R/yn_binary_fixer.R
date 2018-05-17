#' Factorise binary (Y/N) variables
#'
#' Convert numerical variable into factor Y/N
#' @param x numerical variable with 2 levels - 0, 1
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' yn_binary_fixer(p$renalfail)
#' }


yn_binary_fixer <- function(x) {

# check variable class
	if (! is.numeric(x)) {
		stop("Y/N variable not classed as numeric")
	}

# check levels - confirm Y/N status
	if (! length(table(x))) {
		stop("Number of levels of Y/N variable is not 2")
	}

# factorise
	x <- factor(x, levels = c(0,1),
			labels = c("No", "Yes"))

return(x)
}
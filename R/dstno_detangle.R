#' Detangle DST identification number
#'
#' Convert character DST number found in Chechen data to numerical
#' @param x data frame containing APID variable from KK programme
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @import stringr
#' @examples
#' \dontrun{
#' dstno_detangle(p)
#' }


dstno_detangle <- function(x, ...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check variables are present
	v <- c("dstno")

	if (! all(v %in% names(x))) {
		stop("DST number variable not available")
	}

# check all variables are dates
	if (! is.character(x[[v]])) {
		stop("DST number variable is not classed as character")
	}

# number of NAs in original variable
	n <- sum(is.na(x[[v]]))

# remove special character
	x[[v]] <- str_replace(x[[v]], "-", "")

# convert to numerical
	x[[v]] <- as.numeric(x[[v]])

# message to mention how many NAs introduced
	m <- sum(is.na(x[[v]]))
	p <- m - n
	message(paste0("NAs introduced = ", p))

return(x)	
}
#' Aggregate dates in Chechen lab database
#'
#' Consolidate sample dates in Chechen mycobacterial lab database
#' @param x data frame containing sample date variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @import lubridate
#' @examples
#' \dontrun{
#' chechnya_lab_date_consolidator(p, set = "msc500")
#' }


chechnya_lab_date_consolidator <- function(x, ...) {
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check variables are present
	v <- c("dcol1", "dcol2", "dcol3")

	if (! all(v %in% names(x))) {
		stop("All date variables not available - check set arg")
	}

# check all variables are dates
	if (! all(as.logical(lapply(x[ , v], is.Date)))) {
		stop("All variables are not classed as dates")
	}

# aggregate sample collection date
	x$samp_date <- pmin(x$dcol1, x$dcol2, x$dcol3, na.rm = T)

# remove old variables
	x <- x[ , !(names(x) %in% v)]

return(x)
}
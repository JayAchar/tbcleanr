#' Fix Koch 6 baseline viral hepatitis variables
#'
#' Combine Koch 6 HBV and HCV baseline variables and output  
#' integer varialbe 
#' @param x data frame containing Koch 6 HBV and HCV variables
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' k6_hep_fixer(p)
#' }

k6_hep_fixer <- function(x) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# outcome variables in Koch 6
	v <- c("hepB", "hepC", "HepB", "HepC")

	if (! all(v %in% names(x))) {
		warning("Check all viral hepatitis variables present in data frame")
		return(x)
	}

# generate empty variables
	x$hbv <- NA
	x$hcv <- NA

# aggregate hbv
	p <- which(x$hepB == 1 | x$HepB == 1)
	n <- which(x$hepB == 0 & x$HepB == 2)
	x$hbv[p] <- 1L
	x$hbv[n] <- 0L

# aggregate hcv
	p <- which(x$hepC == 1 | x$HepC == 1)
	n <- which(x$hepC == 0 & x$HepC == 2)
	x$hcv[p] <- 1L
	x$hcv[n] <- 0L

# remove original variables
	x$hepB <- NULL
	x$HepB <- NULL
	x$hepC <- NULL
	x$HepC <- NULL

return(x)
}

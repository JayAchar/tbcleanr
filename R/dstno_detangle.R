#' Detangle DST identification number
#'
#' Convert character DST number found in Chechen data to numerical
#' @param x data frame containing APID variable from KK programme
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom stringr str_replace
#' @examples
#' \dontrun{
#' dstno_detangle(p)
#' }


dstno_detangle <- function(x, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"), 
								...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}
# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)

# =======================================================
# check variables are present
	# set db specific variables 
		if (software == "excel" && project == "chechnya" && file == "lab") {
			v <- c("dstno")
		} else {
			return(x)
		}
# =======================================================
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
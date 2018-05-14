#' Fix Koch 6 registration number variable
#'
#' Deconstruct Koch 6 registration number into constiuent parts 
#' and output numerical variable for idno and string for identifier, 
#' if relevant
#' @param x data frame containing Koch 6 registration number
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' k6_idno_fixer(p)
#' }


k6_idno_fixer <- function(x) {
	require(stringr, quietly = T)

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# idno variables in Koch 6
	i <- "registrationnb"

	if (! all(i %in% names(x))) {
		stop("Check registration number variable is included in data frame")
	}

# check if all string identifiers are teh same
	z <- str_match(x$registrationnb, "^.{3}")
	if (length(unique(z)) == 1) {
		message("All string identifiers in registration number variable identical and therefore dropped")
	dummy <- 0
	} else {
# keep ID string if useful identifier
		x$idstring <- as.character(z)
	dummy <- 1
	}

# extract numerical part of idno
	x$idno <- NA
		# extract all digits after characters
	x$idno <- str_extract(x$registrationnb, "\\B\\d+$")
	x$idno <- as.numeric(x$idno)


# check uniqueness of id number
	if (dummy == 0 && ! length(unique(x$idno)) == dim(x)[1]) {
			warning("idno is not a unique identifier in this data set")
	}

### need to add option for dummy == 1

# remove original registration number variable
	x$registrationnb <- NULL

return(x)
}
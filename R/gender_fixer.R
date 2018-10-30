#' Factorise gender variable
#'
#' Take gender variable from data frame and factorise. Requires that variable names
#' have been pre-adjusted.  
#' @param x data frame containing Koch 6 admission variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertthat assert_that
#' @examples
#' # data from Koch 6 with variable names pre-cleaned
#' 
#' x <- tbcleanr:::gender_k6
#' gender_fixer(x)
#' 


gender_fixer <- function(x, ...) {

# check input
        assert_that(is.data.frame(x))

# =================================================================
# check gender variable present
	if (! "gender" %in% names(x)) {
		stop("Gender variable not present in data frame")
	}

# for EpiInfo data
if (is.character(x$gender)) {
# convert character to numeric
	x$gender[x$gender == "M"] <- 1
	x$gender[x$gender == "F"] <- 2
}

# for Koch 6 data
if (is.numeric(x$gender)) {
	# convert gender == not done to NA
	x$gender[x$gender == 0] <- NA
}

# check levels of gender variable
	if (! length(table(x$gender)) == 2) {
		stop("Gender variable does not have 2 levels")
	}

# factorise gender variable
	x$gender <- factor(x$gender, levels = c(1:2),
				labels = c("Male", "Female"))

x
}
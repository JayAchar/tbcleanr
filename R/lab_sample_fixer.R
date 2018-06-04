#' Lab sample fixer
#'
#' Consolidate mycobacterial sample types in Chechen lab data
#' @param x data frame containing sample date variables
#' @param set define variable set to apply. Values can be "chechnya_myco_lab".
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @import stringr
#' @examples
#' \dontrun{
#' lab_sample_fixer(p, rm_orig = TRUE)
#' }

lab_sample_fixer <- function(x, set = "chechnya_myco_lab", rm_orig = TRUE, ...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

s <- c("chechnya_myco_lab")

# check set is within acceptable values
	if (! set %in% s) {
		stop("Specify set argument within specified values")
	}

# check variables are present
	if (set == "chechnya_myco_lab") {
		v <- c("sputum")
	}

	if (! all(v %in% names(x))) {
		stop("Sample type variable not available")
	}

# check variable is.character
	if (! is.character(x[[v]])) {
		stop("Sample type variable is not a string variable")
	}

# ==========================================
# new variable for EPTB and sputum specimens
	x$sample <- NA

	# find EPTB specimens
		e <- which(str_detect(x[[v]], "EPTB"))
	# find sputum specimens
		s <- which(str_detect(x[[v]], "Sputum"))

# recode sample variable
	x$sample[s] <- 1
	x$sample[e] <- 2

# factorise sample variable
	x$sample <- factor(x$sample, levels = 1:2, 
					labels = c("Sputum", "EP sample"))

# how many NA's introduced
	sp <- sum(is.na(x[[v]]))
	sa <- sum(is.na(x$sample))
	p <- sa - sp
	message(paste0("NAs introduced = ", p))

# remove original variable
	if (rm_orig == TRUE) {
		x[[v]] <- NULL
	}

return(x)
}
#' Lab sample fixer
#'
#' Consolidate mycobacterial sample types in Chechen lab data
#' @param x data frame containing sample date variables
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom stringr str_detect
#' @examples
#' \dontrun{
#' lab_sample_fixer(p, rm_orig = TRUE)
#' }

lab_sample_fixer <- function(x, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"), 
								rm_orig = TRUE, ...) {

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
	if (software == "excel" && project == "chechnya" && file == "lab")  {
		v <- c("sputum")
	} else {
		return(x)
	}
# =======================================================

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

x
}
#' Recode culture results
#'
#' Recode individual culture variables
#' @param cult culture variable as defined in result_consolidator()
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
#' @examples
#' \dontrun{
#' culture_recode(cult)
#' }


culture_recode <- function(cult, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"), 
								...) {
# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)

# ====================================================================
if (software == "excel" && project == "chechnya" && file == "lab") {
	# lower case to simplify recoding
	cult <- tolower(cult)

# recode smear variable
	cult[cult %in% c("#ref!", "contaminated")] <- NA
	cult[cult %in% c("negative", "neg")] <- 0
	cult[cult %in% c("positive", "pos")] <- 1

# ====================================================================
} else if (software %in% c("excel", "epiinfo") && project == "kk" && file == "lab") {
	# remove contaminated
	cult[cult == 3] <- NA

	# NEG = 0
	cult[cult == 1] <- 0

	# POS = 1
	cult[cult == 2] <- 1

# ====================================================================	
} else {

	return(cult)
}




cult <- as.numeric(cult)
cult
}

#' Recode smear results
#'
#' Recode individual smear variables
#' @param sm smear variable as defined in result_consolidator()
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
#' smear_recode(sm)
#' }


smear_recode <- function(sm, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"), 
								...) {

# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)
# ====================================================================
	if (software == "excel" && project == "chechnya" && file == "lab") {
					# recode smear variable
					if (! is.character(sm)) {
					stop("Smear variable should be is.character")
				}

	sm[sm %in% c("Not_done", "Not_Done")] <- NA
	sm[sm == "Negative"] <- 0
	sm[sm == "Scanty"] <- 1
	sm[sm == "1+"]	   <- 1
	sm[sm == "2+"]	   <- 2
	sm[sm == "3+"]     <- 3

} else if (software %in% c("excel", "epiinfo") && project == "kk" && file == "lab") {
	
	# nukus lab data smear recode
	sm[sm == 5] <- 1
	sm[sm == 6] <- 0
	sm[sm %in% c(7, 8, 9)] <- NA

} else {

	return(sm)

}


sm <- as.numeric(sm)
sm
}
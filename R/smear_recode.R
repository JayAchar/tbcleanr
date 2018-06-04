#' Recode smear results
#'
#' Recode individual smear variables
#' @param sm smear variable as defined in result_consolidator()
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' smear_recode(sm)
#' }


smear_recode <- function(sm) {
	smear_sets <- "chechnya_myco_lab"

# check var is.character
	if (! is.character(sm)) {
		stop("Smear variable should be is.character")
	}
# check number of levels of smear variable
	if (length(table(sm)) > 6) {
		stop("Check the number of smear variable levels")
	}

# recode smear variable
	sm[sm %in% c("Not_done", "Not_Done")] <- NA
	sm[sm == "Negative"] <- 0
	sm[sm == "Scanty"] <- 1
	sm[sm == "1+"]	   <- 1
	sm[sm == "2+"]	   <- 2
	sm[sm == "3+"]     <- 3

sm <- as.numeric(sm)
return(sm)
}
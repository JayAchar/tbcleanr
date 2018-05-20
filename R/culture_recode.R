#' Recode culture results
#'
#' Recode individual culture variables
#' @param cult culture variable as defined in result_consolidator()
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{TB.funs}}
#' @export
#' @examples
#' \dontrun{
#' culture_recode(cult)
#' }


culture_recode <- function(cult) {
	culture_sets <- "chechnya_myco_lab"

# check var is.character
	if (! is.character(cult)) {
		stop("Culture variable should be is.character")
	}
# check number of levels of smear variable
	if (length(table(cult)) > 5) {
		stop("Check the number of culture variable levels")
	}

# lower case to simplify recoding
	cult <- tolower(cult)


# recode smear variable
	cult[cult %in% c("#ref!", "contaminated")] <- NA
	cult[cult %in% c("negative", "neg")] <- 0
	cult[cult %in% c("positive", "pos")] <- 1

cult <- as.numeric(cult)
return(cult)
}

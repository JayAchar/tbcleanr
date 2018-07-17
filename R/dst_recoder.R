#' Recode DST results
#'
#' Recode individual DST variables
#' @param dst_var DST variable 
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' dst_recoder(dst_var)
#' }


dst_recoder <- function(dst_var, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab")) {

# ===================================================
# Recode DST variable	
if (software == "excel" && project == "chechnya" && file == "lab") {
	if (! is.character(dst_var)) {
			stop("DST variable should be is.character")
		}
			# check number of levels of smeDSTar variable
	if (length(table(dst_var)) > 2) {
			stop("Check the number of DST variable levels")
		}

		dst_var[dst_var == "R"] <- 1L
		dst_var[dst_var == "S"] <- 0L

} else if (software %in% c("excel", "epiinfo") && project == "kk" && file == "lab") {

	if (is.numeric(dst_var)) {
		# NA
		dst_var[! dst_var %in% c(1, 2)] <- NA
		# sensitive
		dst_var[dst_var == 2] <- 0L
	
	} else if (is.character(dst_var)) {
		
		dst_var[! dst_var %in% c("1", "2")] <- NA
		dst_var[dst_var == "2"] <- 0L

	}
		
}

# convert to numerical
	dst_var <- as.numeric(dst_var)

dst_var
}
#' Claen miscellaneous Epi Info TB variables
#'
#' Take data frame of TB admission data from Epi Info
#' and clean specific miscellaneous variables
#' @param x data frame containing Koch 6 admission variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' epi_info_misc_cleanr(p)
#' }


epi_info_misc_cleanr <- function(x, ...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# =====================================
	# factorise employment variable
		if ("EMPL" %in% names(x)) {
			if (length(table(x[["EMPL"]])) > 7){
				stop("EMPL variable has too many levels")
			}
			labs <- c("Employed", "Retired", "Student", "Unemployed", "Housework",
						"Other", "Disabled")
			x[["EMPL"]] <- factor(x[["EMPL"]], levels = c(1:7),
								labels = labs)
		}
# =====================================
	# factorise registration group variable
		if ("REGRP" %in% names(x)) {
			if (length(table(x[["REGRP"]])) > 8){
				stop("REGRP variable has too many levels")
			}			
			labs <- c("New", "Relapse", "Tx after LTFU", "Tx after failing Cat 1", 					"Tx after failing Cat 2", "Transfer In", "Other", "Amplified after failure")
			x[["REGRP"]] <- factor(x[["REGRP"]], levels = c(1:8),
								labels = labs)			
		}
# =====================================
	# factorise site of diseasevariable
		if ("SITEDIS" %in% names(x)) {
			if (length(table(x[["SITEDIS"]])) > 3){
				stop("SITEDIS variable has too many levels")
			}			
			labs <- c("Pulmonary", "EP", "Both")
			x[["SITEDIS"]] <- factor(x[["SITEDIS"]], levels = c(1:3),
								labels = labs)			
		}
# =====================================
	# factorise site of diseasevariable
		if ("PHA" %in% names(x)) {
			if (length(table(x[["PHA"]])) > 2){
				stop("PHA variable has too many levels")
			}			
			labs <- c("Inpatient", "Outpatient")
			x[["PHA"]] <- factor(x[["PHA"]], levels = c(1, 3),
								labels = labs)			
		}
x
}

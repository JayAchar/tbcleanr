#' Complete TB data clean
#'
#' Combine all admission and laboratory data cleaning functions 
#' to organise TB admission and laboratory files into a nested list.
#' @param adm raw admission data frame
#' @param lab raw laboratory data frame#' 
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertive assert_is_data.frame


complete_tb_cleanr <- function(adm, lab,
										software = c("excel", "koch_6", "epiinfo"),
										project = c("kk", "chechnya"),
										...) {

# ================================================================
# check all args
	software <- match.arg(software)
	project <- match.arg(project)

	assert_is_data.frame(adm)
	assert_is_data.frame(lab)

# ================================================================
# clean adm file
	adm <- adm_data_cleanr(adm, software = software, 
															project = project, 
															file = "adm", ...)

# ================================================================
# clean lab file
	lab <- lab_data_cleanr(lab, software = "excel", 
															project = project, 
															file = "lab", ...)

# ================================================================
# generate list for output
	z <- list(adm_clean = adm,
						lab_clean = lab)

z
}
